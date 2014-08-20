{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnboxedTuples #-}

module ForeignDebugLib where

import Foreign.C
import Foreign.Ptr

import GHC.Exts

foreign import ccall unsafe "print_ap_stack" c_print_ap_stack :: Ptr () -> IO ()

foreign import ccall unsafe "print_info_table" c_print_info_table :: Ptr () -> IO ()

printInfoTable :: a -> IO ()
printInfoTable a = case unpackClosure# a of (# addr, _, _ #) -> c_print_info_table (Ptr addr)