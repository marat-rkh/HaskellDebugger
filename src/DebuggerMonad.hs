{-# LANGUAGE CPP, FlexibleInstances, UnboxedTuples, MagicHash #-}
{-# OPTIONS_GHC -fno-cse -fno-warn-orphans #-}

module DebuggerMonad where

import GHC hiding (resume)
import Data.IORef
import Control.Monad
import Exception (ExceptionMonad(..))
import GhcMonad ()
import DynFlags
import MonadUtils
import GhcMonad ()

import ObjLink (lookupSymbol)
import GHC.Ptr ()
import Linker
import GHC.Exts

import System.IO

data DebugState = DebugState {
    mainFile :: Maybe String,
    breaks :: [(Int, Module, Int)], -- number, module, line
    port :: Maybe Int,
    debugOutput :: Handle,
    mb_stdout_ptr :: Maybe (Ptr ()),
    mb_stderr_ptr :: Maybe (Ptr ())
}

initState :: DebugState
initState = DebugState {
    mainFile = Nothing,
    breaks = [],
    port = Nothing,
    debugOutput = stdout,
    mb_stdout_ptr = Nothing,
    mb_stderr_ptr = Nothing
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

initInterpBuffering :: DebuggerMonad ()
initInterpBuffering = do
    dflags <- GHC.getSessionDynFlags
    liftIO $ initDynLinker dflags
    mb_stdout <- liftIO $ ObjLink.lookupSymbol "base_GHCziIOziHandleziFD_stdout_closure"
    mb_stderr <- liftIO $ ObjLink.lookupSymbol "base_GHCziIOziHandleziFD_stderr_closure"
    modifyDebugState $ \st -> st{mb_stdout_ptr = mb_stdout, mb_stderr_ptr = mb_stderr}

flushInterpBuffers :: DebuggerMonad ()
flushInterpBuffers = do
    st <- getDebugState
    let (Just stdout_ptr) = mb_stdout_ptr st
    let (Just stderr_ptr) = mb_stderr_ptr st
    h1 <- liftIO $ getHandle (stdout_ptr)
    h2 <- liftIO $ getHandle (stderr_ptr)
    liftIO $ hFlush h1
    liftIO $ hFlush h2

turnOffBuffering :: DebuggerMonad ()
turnOffBuffering = do
    st <- getDebugState
    let (Just stdout_ptr) = mb_stdout_ptr st
    let (Just stderr_ptr) = mb_stderr_ptr st
    h1 <- liftIO $ getHandle (stdout_ptr)
    h2 <- liftIO $ getHandle (stderr_ptr)
    liftIO $ hSetBuffering h1 NoBuffering
    liftIO $ hSetBuffering h2 NoBuffering

getHandle :: Ptr () -> IO Handle
getHandle (Ptr addr) = do
    case addrToAny# addr of (# hval #) -> return (unsafeCoerce# hval)

