{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Utility.Core (
	c_compress, c_compressBound, c_uncompress
	) where

import Foreign.Ptr
import Data.Word

import Codec.Compression.Zlib.Constant.Core

#include <zlib.h>

foreign import ccall "compress" c_compress ::
	Ptr #{type Bytef} -> Ptr #{type uLongf} ->
	Ptr #{type Bytef} -> #{type uLong} -> IO ReturnCode

foreign import ccall "compressBound" c_compressBound ::
	#{type uLong} -> #{type uLong}

foreign import ccall "uncompress" c_uncompress ::
	Ptr #{type Bytef} -> Ptr #{type uLongf} ->
	Ptr #{type Bytef} -> #{type uLong} -> IO ReturnCode
