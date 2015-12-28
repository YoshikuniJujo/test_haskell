{-# LANGUAGE OverloadedStrings #-}

module Codec (encode, decode, image) where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')
import Data.Word (Word32)
import qualified Data.ByteString as BS

import Zlib (Header, Cmf(..), FLvl(..), decHeader, encHeader)
import IHDR (
	IHDR(..), ColorType(..), HasColor(..), HasAlpha(..), toChunk, frChunk)
import Chunks (Chunk(..), idat, toPng, frPng)
import Bits (complement, (.|.), shiftL, leToByteString, beToByteString)

data PNG = PNG { header :: IHDR, zlib :: Header, body :: BS.ByteString }
	deriving Show

decode :: BS.ByteString -> Maybe PNG
decode = ((\(h, d) -> uncurry . PNG <$> frChunk h <*> decHeader (idat d)) =<<)
	. (uncons =<<) . frPng

encode :: PNG -> BS.ByteString
encode p = toPng . (toChunk (header p) :) . (: []) . Chunk "IDAT" $
	encHeader (zlib p) `BS.append` body p

image :: Int -> Int -> [BS.ByteString] -> PNG
image w h i = PNG {
	header = IHDR {
		width = w, height = h, bitDepth = 8,
		colorType = NoPalette Grayscale NoAlpha,
		compMethod = 0, filterMethod = 0, ilaceMethod = 0 },
	zlib = (Deflate 32768, FLvl 0, Nothing),
	body = noCompress . foldl' BS.append "" $ map (0 `BS.cons`) i }

noCompress :: BS.ByteString -> BS.ByteString
noCompress s = BS.cons 0x01 . (`BS.append` BS.append s (adler32 s))
	. (BS.append <$> id <*> BS.map complement)
	. leToByteString 2 $ BS.length s

adler32 :: BS.ByteString -> BS.ByteString
adler32 = ((beToByteString 4 . uncurry ((. (`shiftL` 16)) . (.|.))) .)
	. flip BS.foldl' (1, 0) $ \(a, b) x ->
		let a' = (a + fromIntegral x) `mod` c in (a', (b + a') `mod` c)
	where c = 65521 :: Word32

uncons :: [a] -> Maybe (a, [a])
uncons (x : xs) = Just (x, xs); uncons _ = Nothing
