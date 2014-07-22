{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Debugger where

import GHC hiding (resume)
import qualified GHC (resume)
import GHC.Paths ( libdir )
import DynFlags
import GhcMonad (liftIO)
import Outputable (Outputable)
import qualified Outputable

import CommandParser (parse, debugCommand, DebugCommand(..))

import System.IO


whileNot :: (Monad m) => m Bool -> m ()
whileNot p = do
    x <- p
    if not x then whileNot p else return ()

mainModulePath :: String
mainModulePath = "Main.hs"

mainModuleName :: String
mainModuleName = "Main"

setupContext :: GhcMonad m => String -> String -> m ()
setupContext pathToModule nameOfModule = do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                , ghcLink   = LinkInMemory
                                }
    setTargets =<< sequence [guessTarget pathToModule Nothing]
    GHC.load LoadAllTargets
    setContext [IIModule $ mkModuleName nameOfModule]

setupStandardContext :: GhcMonad m => m ()
setupStandardContext = setupContext mainModulePath mainModuleName

printSDoc :: GhcMonad m => Handle -> Outputable.SDoc -> m ()
printSDoc handle message = do
    dflags <- getDynFlags
    unqual <- getPrintUnqual
    liftIO $ Outputable.printForUser dflags handle unqual message
    return ()

printOutputable :: (GhcMonad m, Outputable d) => Handle -> d -> m ()
printOutputable handle message = printSDoc handle $ Outputable.ppr message

printString :: (GhcMonad m) => Handle -> String -> m ()
printString handle message = printSDoc handle $ Outputable.text message

trace :: GhcMonad m => String -> m RunResult
trace expr = do
    printString stdout "# Trace started"
    runStmt expr RunAndLogSteps

-- Returns True if debug finished
handleRunResult :: GhcMonad m => RunResult -> m Bool
handleRunResult result = let
    print_ [] = return ()
    print_ (n:ns) = do
        printOutputable stdout n
        print_ ns
    in do
        case result of
            RunOk names -> do
                printString stdout "# Trace finished"
                printString stdout "# Observable names:"
                print_ names
                return True
            RunBreak _ names m_breakInfo -> do
                printString stdout "# Trace paused"
                printString stdout "# Observable names:"
                print_ names
                case m_breakInfo of
                    Just breakInfo -> do
                        let modName = moduleName $ breakInfo_module breakInfo
--                        printString stdout "# Module name:"
--                        printOutputable stdout modName
                        printString stdout "# Line number:"
                        printSDoc stdout $ Outputable.int $ breakInfo_number breakInfo
                    Nothing -> printString stdout "# No break info"
                return False
            _ -> fail "UNHANDLED RESULT"


defaultRunGhc :: Ghc a -> IO ()
defaultRunGhc program = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    setupStandardContext
    program
    return ()

setBreakpoint :: (GhcMonad m) => String -> Int -> m ()
setBreakpoint modName line = do
    md <- GHC.lookupModule (mkModuleName modName) Nothing
    Just mod_info <- getModuleInfo md
    let breaks = modBreaks_flags $ modInfoModBreaks mod_info
    res <- liftIO $ setBreakOn breaks line
    printString stdout $ if res then "# Breakpoint was set at line " ++ show line else "# Breakpoint was not set"
    return ()

resume :: (GhcMonad m) => m RunResult
resume = do
    printString stdout "# Trace resumed"
    GHC.resume (const True) RunAndLogSteps

getCommand :: (GhcMonad m) => Handle -> m DebugCommand
getCommand handle = do
    line <- liftIO $ hGetLine handle
    return $ fst $ head $ parse debugCommand line

-- Returns True if debug finished
runCommand :: (GhcMonad m) => DebugCommand -> m (Bool)
runCommand (SetBreakpoint mod line) = setBreakpoint mod line >> return False
runCommand (Trace command)          = trace command >>= handleRunResult
runCommand (Resume)                 = resume >>= handleRunResult
runCommand _                        = fail "UNHANDLED COMMAND"

main :: IO ()
main = defaultRunGhc $ do
    whileNot (do {command <- getCommand stdin; printString stdout $ show command; runCommand command })
