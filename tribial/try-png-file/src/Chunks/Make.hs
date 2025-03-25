{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.Make where

import Data.Bits
import Chunks.SomeChunk
import Chunks.Core
import Chunks.MagicAndEnd

import Data.ByteString qualified as BS

import Crc
import Tools

sampleIhdr :: Ihdr
sampleIhdr = Ihdr {
	ihdrWidth = 16, ihdrHeight = 16, ihdrDepth = 8,
	ihdrColorType = ColorTypeColor .|. ColorTypeAlpha,
	ihdrCompression = DeflateInflate32, ihdrFilter = FilterMethod0,
	ihdrInterlace = NoInterlace }

sampleIdat0 :: Idat
sampleIdat0 = Idat . replicate 16 . BS.cons 0 . BS.concat $
	replicate 16 "\x30\x70\x30\xff"

sampleIdat1 :: Idat
sampleIdat1 = Idat . replicate 16 . BS.cons 1 . BS.concat $
	"\x10\x10\x10\xff" : replicate 15 "\x10\x10\x10\x0"

sampleIdat2 :: Idat
sampleIdat2 = Idat $ BS.cons 2 (BS.concat (replicate 16 "\x10\x10\x10\xff")) :
	replicate 15 (BS.cons 2 . BS.concat $ replicate 16 "\x10\x10\x10\xff")

sampleIdat3 :: Idat
sampleIdat3 = Idat $ BS.cons 3 (BS.concat ("\x08\x08\x08\xff" : replicate 15 "\x08\x08\x08\x80")) :
	replicate 15 (BS.cons 3 . BS.concat $ "\x08\x08\x08\x80" : replicate 15 "\x08\x08\x08\x0")

sampleIdat4 :: Idat
sampleIdat4 = Idat $ BS.cons 4 (BS.concat (replicate 16 "\x08\x08\x08\xff")) :
	replicate 15 (BS.cons 4 . BS.concat $ replicate 16 "\x08\x08\x08\x0")

sampleIend :: Iend
sampleIend = Iend

samplePng0 :: BS.ByteString
samplePng0 = BS.concat [
	magic, makeChunk sampleIhdr, makeChunk sampleIdat0, makeChunk sampleIend ]

samplePng1 :: BS.ByteString
samplePng1 = BS.concat [
	magic, makeChunk sampleIhdr, makeChunk sampleIdat1, makeChunk sampleIend ]

samplePng2 :: BS.ByteString
samplePng2 = BS.concat [
	magic, makeChunk sampleIhdr, makeChunk sampleIdat2, makeChunk sampleIend ]

samplePng3 :: BS.ByteString
samplePng3 = BS.concat [
	magic, makeChunk sampleIhdr, makeChunk sampleIdat3, makeChunk sampleIend ]

samplePng4 :: BS.ByteString
samplePng4 = BS.concat [
	magic, makeChunk sampleIhdr, makeChunk sampleIdat4, makeChunk sampleIend ]

makeChunk :: forall c . CodecChunkOld c => c -> BS.ByteString
makeChunk c = BS.concat [
	num32ToBs $ BS.length bs,
	chunkNameOld @c,
	bs,
	num32ToBs . crc $ chunkNameOld @c `BS.append` bs ]
	where
	bs = encodeChunkOld c
