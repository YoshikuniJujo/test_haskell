{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Basic.Core (

	zlibVersion,

	deflateInit, deflate, deflateEnd,
	inflateInit, inflate, inflateEnd

	) where

import Foreign.Ptr
import Foreign.C.String
import Control.Monad.Primitive
import System.IO.Unsafe

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core

zlibVersion :: String
zlibVersion = unsafePerformIO $ peekCString c_zlibVersion

deflateInit :: PrimBase m =>
	StreamPrim (PrimState m) -> CompressionLevel -> m ReturnCode
deflateInit strm cl = withStreamPtr strm $ unsafeIOToPrim . (`c_deflateInit` cl)

deflate :: PrimBase m => StreamPrim (PrimState m) -> Flush -> m ReturnCode
deflate strm fls = withStreamPtr strm $ unsafeIOToPrim . (`c_deflate` fls)

deflateEnd :: PrimBase m => StreamPrim (PrimState m) -> m ReturnCode
deflateEnd strm = withStreamPtr strm $ unsafeIOToPrim . c_deflateEnd

inflateInit :: PrimBase m => StreamPrim (PrimState m) -> m ReturnCode
inflateInit strm = withStreamPtr strm $ unsafeIOToPrim . c_inflateInit

inflate :: PrimBase m => StreamPrim (PrimState m) -> Flush -> m ReturnCode
inflate strm fls = withStreamPtr strm \s -> unsafeIOToPrim $ c_inflate s fls

inflateEnd :: PrimBase m => StreamPrim (PrimState m) -> m ReturnCode
inflateEnd strm = withStreamPtr strm $ unsafeIOToPrim . c_inflateEnd

foreign import ccall "zlibVersion" c_zlibVersion :: CString

foreign import capi "zlib.h deflateInit" c_deflateInit :: Ptr Stream -> CompressionLevel -> IO ReturnCode

foreign import ccall "deflate" c_deflate :: Ptr Stream -> Flush -> IO ReturnCode

foreign import ccall "deflateEnd" c_deflateEnd :: Ptr Stream -> IO ReturnCode

foreign import capi "zlib.h inflateInit" c_inflateInit :: Ptr Stream -> IO ReturnCode

foreign import ccall "inflate" c_inflate :: Ptr Stream -> Flush -> IO ReturnCode

foreign import ccall "inflateEnd" c_inflateEnd :: Ptr Stream -> IO ReturnCode
