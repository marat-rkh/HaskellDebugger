import Control.Applicative
import DynFlags
import GHC
import GHC.Paths
import GhcMonad (liftIO) 
    -- from ghc7.7 and up you can use the usual
    -- liftIO from Control.Monad.IO.Class
import Unsafe.Coerce

--execWithModule :: String -> String -> String -> IO ()
execWithModule pathToModule nameOfModule expr = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        -- we have to call 'setSessionDynFlags' before doing
        -- everything else
        dflags <- getSessionDynFlags
        -- If we want to make GHC interpret our code on the fly, we
                  -- ought to set those two flags, otherwise we
                  -- wouldn't be able to use 'setContext' below
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [guessTarget pathToModule Nothing]
        load LoadAllTargets
        -- Bringing the module into the context
        setContext [IIModule $ mkModuleName nameOfModule]

        -- evaluating and running an action
        act <- unsafeCoerce <$> compileExpr expr           
        liftIO act