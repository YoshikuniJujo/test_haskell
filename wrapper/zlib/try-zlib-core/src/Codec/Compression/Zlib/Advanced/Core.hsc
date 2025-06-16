{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Advanced.Core where

import Foreign.Ptr
import Data.Int

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core

newtype WindowBits = WindowBits #{type int} deriving Show

pattern WindowBitsZlibHeader :: WindowBits
pattern WindowBitsZlibHeader = WindowBits 0

pattern WindowBitsRaw :: #{type int} -> WindowBits
pattern WindowBitsRaw bs <- WindowBits (negate -> bs) where
	WindowBitsRaw bs = WindowBits $ negate bs

pattern WindowBitsGzip :: #{type int} -> WindowBits
pattern WindowBitsGzip bs <- WindowBits (subtract 16 -> bs) where
	WindowBitsGzip bs = WindowBits $ bs + 16

pattern WindowBitsZlibAndGzip :: #{type int} -> WindowBits
pattern WindowBitsZlibAndGzip bs <- WindowBits (subtract 32 -> bs) where
	WindowBitsZlibAndGzip bs = WindowBits $ bs + 32

foreign import capi "zlib.h inflateInit2" c_inflateInit2 ::
	Ptr Stream -> WindowBits -> IO ReturnCode
