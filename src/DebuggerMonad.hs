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
    port :: Maybe Int,
    debugOutput :: Handle
}

initState :: DebugState
initState = DebugState {
    breaks = [],
    port = Nothing,
    debugOutput = stdout
}

newtype DebuggerMonad a = DebuggerMonad {toGhc :: IORef DebugState -> Ghc a}

getDebugState :: DebuggerMonad DebugState
getDebugState   = DebuggerMonad $ \r -> liftIO $ readIORef r
setDebugState :: DebugState -> DebuggerMonad ()
setDebugState s = DebuggerMonad $ \r -> liftIO $ writeIORef r s
modifyDebugState :: (DebugState -> DebugState) -> DebuggerMonad ()
modifyDebugState f = DebuggerMonad $ \r -> liftIO $ readIORef r >>= writeIORef r . f

liftGhc :: Ghc a -> DebuggerMonad a
liftGhc m = DebuggerMonad $ \_ -> m


startDebugger :: DebuggerMonad a -> DebugState -> Ghc a
startDebugger g state = do ref <- liftIO $ newIORef state; toGhc g ref


instance MonadIO DebuggerMonad where
    liftIO = liftGhc . liftIO

instance Monad DebuggerMonad where
    (DebuggerMonad m) >>= k = DebuggerMonad $ \s -> m s >>= \a -> toGhc (k a) s
    return a = DebuggerMonad $ \_ -> return a

instance Functor DebuggerMonad where
    fmap = liftM

instance HasDynFlags DebuggerMonad where
    getDynFlags = getSessionDynFlags

instance GhcMonad DebuggerMonad where
    setSession s' = liftGhc $ setSession s'
    getSession    = liftGhc $ getSession

instance ExceptionMonad DebuggerMonad where
    gcatch m h = DebuggerMonad $ \r -> toGhc m r `gcatch` (\e -> toGhc (h e) r)
    gmask f =
        DebuggerMonad $ \s -> gmask $ \io_restore ->
            let
                g_restore (DebuggerMonad m) = DebuggerMonad $ \s' -> io_restore (m s')
            in
                toGhc (f g_restore) s
