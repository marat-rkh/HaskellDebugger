{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Debugger where

import GHC
import GHC.Paths ( libdir )
import DynFlags

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

trace :: GhcMonad m => String -> m RunResult
trace expr = runStmt expr RunAndLogSteps

runCommand :: Ghc a -> IO ()
runCommand command = defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
    setupStandardContext
    command
    return ()

main :: IO ()
main = runCommand $ trace "main"