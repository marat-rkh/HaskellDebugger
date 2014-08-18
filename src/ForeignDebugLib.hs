{-# LANGUAGE ForeignFunctionInterface #-}
{-# INCLUDE "c_src/DebugLibrary.h" #-}

module ForeignDebugLib where

import Foreign.C

foreign import ccall unsafe "test_lib" c_test_lib :: CInt -> IO CInt