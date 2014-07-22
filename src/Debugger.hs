{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Debugger where

import GHC
import GHC.Paths ( libdir )
import DynFlags
import Outputable (Outputable)
import GhcMonad (liftIO)
import qualified GhcMonad
import qualified Outputable
import System.IO

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
    load LoadAllTargets
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

trace :: GhcMonad m => String -> m ()
trace expr = do
    printString stdout "# Trace started"
    result <- runStmt expr RunAndLogSteps
    afterRun result

afterRun :: GhcMonad m => RunResult -> m()
afterRun result = let
    print_ [] = return ()
    print_ (n:ns) = do
        printOutputable stdout n
        print_ ns
    in do
        case result of
            RunOk names -> do
                printString stdout "# Trace finished."
                printString stdout "# Observable names: "
                print_ names
            RunBreak _ names m_breakInfo -> do
                printString stdout "# Trace stopped at line/breakpoint."
                printString stdout "# Observable names: "
                print_ names
                case m_breakInfo of
                    Just breakInfo -> do
                        let modName = moduleName $ breakInfo_module breakInfo
                        printString stdout "# Module name:"
                        printOutputable stdout modName
                        printString stdout "# Line number:"
                        printSDoc stdout $ Outputable.int $ breakInfo_number breakInfo
                    Nothing -> printString stdout "# No break info."
            otherwise -> fail "UNHANDLED RESULT"


runCommand :: Ghc a -> IO ()
runCommand command = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    setupStandardContext
    command
    return ()

setBreakpoint :: (GhcMonad m) => String -> Int -> m ()
setBreakpoint modName line = do
    md <- GHC.lookupModule (mkModuleName modName) Nothing
    Just mod_info <- getModuleInfo md
    let breaks = modBreaks_flags $ modInfoModBreaks mod_info
    res <- liftIO $ setBreakOn breaks line
    printString stdout $ if res then "# Breakpoint was set at line " ++ show line else "# Breakpoint was not set"
    return ()


main :: IO ()
main = runCommand $ do
    setBreakpoint mainModuleName 9
    -- Somehow trace stops on line 11 of Main.hs ('foo a = 1'), if the corresponding breakpoint is set:
    -- setBreakpoint mainModuleName 11
    trace "main"
