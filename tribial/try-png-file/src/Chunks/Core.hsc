{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.Core where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Enum
import Data.Bits
import Data.Word
import System.IO.Unsafe

import Chunks.SomeChunk

import Data.ByteString qualified as BS

#include "chunks.h"

newtype Word32BE = Word32BE Word32
	deriving (Show, Bits, Eq, Ord, Num, Integral, Real, Enum)

instance Storable Word32BE where
	sizeOf _ = sizeOf @Word32 undefined
	alignment _ = alignment @Word32 undefined
	peek p = bigEndian 0 <$> peekArray 4 (castPtr p)
	poke p w = pokeArray (castPtr p) (replicate (4 - length bs) 0 ++ bs)
		where
		bs = bigEndianEncode [] w

bigEndian :: (Bits n, Integral n) => n -> [Word8] -> n
bigEndian r [] = r
bigEndian r (b : bs) = bigEndian (r `shiftL` 8 .|. fromIntegral b) bs

bigEndianEncode :: (Bits n, Integral n) => [Word8] -> n -> [Word8]
bigEndianEncode r 0 = r
bigEndianEncode r n = bigEndianEncode ((fromIntegral n .&. 0xff) : r) (n `shiftR` 8)

enum "ColorType" ''#{type uint8_t} [''Show, ''Read, ''Eq, ''Storable] [
	("Pallet", #{const Pallet}),
	("Color", #{const Color}),
	("Alpha", #{const Alpha}) ]

enum "CompressionMethod" ''#{type uint8_t} [''Show, ''Read, ''Eq, ''Storable] [
	("DeflateInflate32", #{const DeflateInflate32}) ]

enum "FilterMethod" ''#{type uint8_t} [''Show, ''Read, ''Eq, ''Storable] [
	("FilterMethod0", #{const FilterMethod0}) ]

enum "InterlaceMethod" ''#{type uint8_t} [''Show, ''Read, ''Eq, ''Storable] [
	("NoInterlace", #{const NoInterlace}),
	("Adam7Interlace", #{const Adam7Interlace}) ]

struct "Ihdr" #{size chunk_ihdr} #{alignment chunk_ihdr} [
	("width", ''Word32BE,
		[| #{peek chunk_ihdr, width} |],
		[| #{poke chunk_ihdr, width} |]),
	("height", ''Word32BE,
		[| #{peek chunk_ihdr, height} |],
		[| #{poke chunk_ihdr, height} |]),
	("depth", ''#{type uint8_t},
		[| #{peek chunk_ihdr, depth} |],
		[| #{poke chunk_ihdr, depth} |]),
	("colorType", ''ColorType,
		[| #{peek chunk_ihdr, color_type} |],
		[| #{poke chunk_ihdr, color_type} |]),
	("compression", ''CompressionMethod,
		[| #{peek chunk_ihdr, compression} |],
		[| #{poke chunk_ihdr, compression} |]),
	("filter", ''FilterMethod,
		[| #{peek chunk_ihdr, compression} |],
		[| #{poke chunk_ihdr, compression} |]),
	("interlace", ''InterlaceMethod,
		[| #{peek chunk_ihdr, interlace} |],
		[| #{poke chunk_ihdr, interlace} |])
	]
	[''Show, ''Storable]

instance CodecChunk Ihdr where
	chunkName = "IHDR"
	decodeChunk bs = pure . unsafePerformIO $ BS.useAsCStringLen bs \(pbs, pln) -> do
		p <- malloc
		copyBytes (castPtr p) pbs (min pln $ sizeOf (undefined :: Ihdr))
		Ihdr_ <$> newForeignPtr p (free p)
	encodeChunk (Ihdr_ fp) = unsafePerformIO $ withForeignPtr fp \p ->
		BS.packCStringLen (castPtr p, sizeOf (undefined :: Ihdr))

instance Chunk Ihdr
