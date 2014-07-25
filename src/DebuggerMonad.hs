module DebuggerMonad where

import GHC hiding (resume)
import Data.IORef
import Control.Monad
import Exception (ExceptionMonad(..))
import GhcMonad ()
import DynFlags
import MonadUtils

import System.IO


data DebugState = DebugState {
    breaks :: [(Int, Module, Int)], -- number, module, line
    debugOutput :: Handle
}

initState :: DebugState
initState = DebugState {
    breaks = [],
    debugOutput = stdout -- temporary handle for testing
}

newtype Debugger a = Debugger {toGhc :: IORef DebugState -> Ghc a}

getDebugState :: Debugger DebugState
getDebugState   = Debugger $ \r -> liftIO $ readIORef r
setDebugState :: DebugState -> Debugger ()
setDebugState s = Debugger $ \r -> liftIO $ writeIORef r s
modifyDebugState :: (DebugState -> DebugState) -> Debugger ()
modifyDebugState f = Debugger $ \r -> liftIO $ readIORef r >>= writeIORef r . f

liftGhc :: Ghc a -> Debugger a
liftGhc m = Debugger $ \_ -> m


startDebugger :: Debugger a -> DebugState -> Ghc a
startDebugger g state = do ref <- liftIO $ newIORef state; toGhc g ref


instance MonadIO Debugger where
    liftIO = liftGhc . liftIO

instance Monad Debugger where
    (Debugger m) >>= k = Debugger $ \s -> m s >>= \a -> toGhc (k a) s
    return a = Debugger $ \_ -> return a

instance Functor Debugger where
    fmap = liftM

instance HasDynFlags Debugger where
    getDynFlags = getSessionDynFlags

instance GhcMonad Debugger where
    setSession s' = liftGhc $ setSession s'
    getSession    = liftGhc $ getSession

instance ExceptionMonad Debugger where
    gcatch m h = Debugger $ \r -> toGhc m r `gcatch` (\e -> toGhc (h e) r)
    gmask f =
        Debugger $ \s -> gmask $ \io_restore ->
            let
                g_restore (Debugger m) = Debugger $ \s' -> io_restore (m s')
            in
                toGhc (f g_restore) s
