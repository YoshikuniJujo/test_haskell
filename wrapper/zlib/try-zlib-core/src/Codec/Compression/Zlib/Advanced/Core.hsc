{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Advanced.Core (

	deflateInit2,
	inflateInit2,

	WindowBits,
	pattern WindowBitsZlibHeader,
	pattern WindowBitsZlib, pattern WindowBitsRaw,
	pattern WindowBitsGzip, pattern WindowBitsZlibAndGzip,

	) where

import Foreign.Ptr
import Control.Monad.Primitive
import Data.Word.ToolsYj
import Data.Int

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core

newtype WindowBits = WindowBits #{type int} deriving Show

pattern WindowBitsZlibHeader :: WindowBits
pattern WindowBitsZlibHeader = WindowBits 0

pattern WindowBitsZlib :: Word4 -> WindowBits
pattern WindowBitsZlib bs <- WindowBits (fromIntegral -> bs) where
	WindowBitsZlib bs = WindowBits $ fromIntegral bs

pattern WindowBitsRaw :: Word4 -> WindowBits
pattern WindowBitsRaw bs <- WindowBits (fromIntegral . negate -> bs) where
	WindowBitsRaw bs = WindowBits . negate $ fromIntegral bs

pattern WindowBitsGzip :: Word4 -> WindowBits
pattern WindowBitsGzip bs <- WindowBits (fromIntegral . subtract 16 -> bs) where
	WindowBitsGzip bs = WindowBits $ fromIntegral bs + 16

pattern WindowBitsZlibAndGzip :: Word4 -> WindowBits
pattern WindowBitsZlibAndGzip bs <- WindowBits (fromIntegral . subtract 32 -> bs) where
	WindowBitsZlibAndGzip bs = WindowBits $ fromIntegral bs + 32

deflateInit2 :: PrimBase m =>
	StreamPrim (PrimState m) -> CompressionLevel -> CompressionMethod ->
	WindowBits -> MemLevel -> CompressionStrategy -> m ReturnCode
deflateInit2 strm lvl mtd wbs ml str = withStreamPtr strm
	$ unsafeIOToPrim . (\pstrm -> c_deflateInit2 pstrm lvl mtd wbs ml str)

foreign import capi "zlib.h deflateInit2" c_deflateInit2 ::
	Ptr Stream -> CompressionLevel -> CompressionMethod -> WindowBits ->
	MemLevel -> CompressionStrategy -> IO ReturnCode

inflateInit2 :: PrimBase m =>
	StreamPrim (PrimState m) -> WindowBits -> m ReturnCode
inflateInit2 strm wbs =
	withStreamPtr strm $ unsafeIOToPrim . (`c_inflateInit2` wbs)

foreign import capi "zlib.h inflateInit2" c_inflateInit2 ::
	Ptr Stream -> WindowBits -> IO ReturnCode
