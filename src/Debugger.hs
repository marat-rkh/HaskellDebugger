{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Debugger where

import GHC hiding (resume)
import qualified GHC (resume)
import GHC.Paths ( libdir )
import DynFlags
import qualified GhcMonad (liftIO)
import Outputable (Outputable)
import qualified Outputable

import CommandParser (parse, debugCommand, DebugCommand(..))
import DebuggerMonad

import System.IO

debugOutput :: Handle
debugOutput = stdout

whileNot :: (Monad m) => m Bool -> m ()
whileNot p = do
    x <- p
    if not x then whileNot p else return ()

-- |Test module file
mainModulePath :: String
mainModulePath = "Main.hs"

-- |Test module name
mainModuleName :: String
mainModuleName = "Main"

-- |Setup needed flags before running debug program
setupContext :: String -> String -> Debugger ()
setupContext pathToModule nameOfModule = do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                , ghcLink   = LinkInMemory
                                }
    setTargets =<< sequence [guessTarget pathToModule Nothing]
    GHC.load LoadAllTargets
    setContext [IIModule $ mkModuleName nameOfModule]

-- |Context for test file
setupStandardContext :: Debugger ()
setupStandardContext = setupContext mainModulePath mainModuleName

-- |Runs Ghc program with default settings
defaultRunGhc :: Debugger a -> IO ()
defaultRunGhc program = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $
    startDebugger (do
        setupStandardContext
        program
        return ()
    ) initState

-- |Prints SDoc to a given stream
printSDoc :: Handle -> Outputable.SDoc -> Debugger ()
printSDoc handle message = do
    dflags <- getDynFlags
    unqual <- getPrintUnqual
    GhcMonad.liftIO $ Outputable.printForUser dflags handle unqual message
    return ()

-- |Prints Outputable to a given stream
printOutputable :: (Outputable d) => Handle -> d -> Debugger ()
printOutputable handle message = printSDoc handle $ Outputable.ppr message

-- |Prints String to a given stream
printString :: Handle -> String -> Debugger ()
printString handle message = printSDoc handle $ Outputable.text message

-- |Runs trace of a given command
trace :: String -> Debugger RunResult
trace expr = do
    printString debugOutput "# Trace started"
    res <- liftGhc $ runStmt expr RunAndLogSteps
    return res

-- |Outputs run results and returns True iff debug finished
handleRunResult :: RunResult -> Debugger Bool
handleRunResult result = let
    print_ [] = return ()
    print_ (n:ns) = do
        printOutputable debugOutput n
        print_ ns
    in do
        case result of
            RunOk names -> do
                printString debugOutput "# Trace finished"
                printString debugOutput "# Observable names:"
                print_ names
                return True
            RunBreak _ names m_breakInfo -> do
                printString debugOutput "# Trace paused"
                printString debugOutput "# Observable names:"
                print_ names
                case m_breakInfo of
                    Just breakInfo -> do
                        --let modName = moduleName $ breakInfo_module breakInfo
                        --printString debugOutput "# Module name:"
                        --printOutputable debugOutput modName
                        printString debugOutput "# Line number:"
                        printSDoc debugOutput $ Outputable.int $ breakInfo_number breakInfo
                    Nothing -> printString debugOutput "# No break info"
                return False
            _ -> fail "UNHANDLED RESULT"

-- |Sets breakpoint at given module and line
setBreakpoint :: String -> Int -> Debugger ()
setBreakpoint modName line = do
    md <- liftGhc $ GHC.lookupModule (mkModuleName modName) Nothing
    Just mod_info <- getModuleInfo md
    let breaks = modBreaks_flags $ modInfoModBreaks mod_info
    res <- liftGhc $ GhcMonad.liftIO $ setBreakOn breaks line
    printString debugOutput $ if res then "# Breakpoint was set at line " ++ show line else "# Breakpoint was not set"
    return ()

-- |Resume program after stopped
resume :: Debugger RunResult
resume = do
    printString debugOutput "# Trace resumed"
    liftGhc $ GHC.resume (const True) RunAndLogSteps

-- |Translates line from input stream into DebugCommand
getCommand :: Handle -> Debugger DebugCommand
getCommand handle = do
    line <- GhcMonad.liftIO $ hGetLine handle
    return $ fst $ head $ parse debugCommand line

-- |Runs DebugCommand and returns True iff debug is finished
runCommand :: DebugCommand -> Debugger (Bool)
runCommand (SetBreakpoint mod line) = setBreakpoint mod line >> return False
runCommand (Trace command)          = trace command >>= handleRunResult
runCommand (Resume)                 = resume >>= handleRunResult
runCommand _                        = printString debugOutput "# Unknown command" >> return False

-- |In loop waits for commands and executes them
startCommandLine :: Debugger ()
startCommandLine = whileNot (do {command <- getCommand stdin; runCommand command })

