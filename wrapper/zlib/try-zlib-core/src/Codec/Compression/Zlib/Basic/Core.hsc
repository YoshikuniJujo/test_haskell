{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Basic.Core where

import Foreign.Ptr
import Foreign.C.String

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core

foreign import ccall "zlibVersion" c_zlibVersion :: CString

foreign import capi "zlib.h inflateInit" c_inflateInit :: Ptr Stream -> IO ReturnCode

foreign import ccall "inflate" c_inflate :: Ptr Stream -> Flush -> IO ReturnCode

foreign import ccall "inflateEnd" c_inflateEnd :: Ptr Stream -> IO ReturnCode
