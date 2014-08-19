{-# LANGUAGE ForeignFunctionInterface #-}

module ForeignDebugLib where

import Foreign.C
import Foreign.Ptr

foreign import ccall unsafe "print_hvalue" c_print_hvalue :: Ptr () -> IO ()