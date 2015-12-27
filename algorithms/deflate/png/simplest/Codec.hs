{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Codec where

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.List
import Data.Bits
import qualified Data.ByteString as BS
import System.IO
import System.IO.Unsafe

import Chunks
import IHDR
import qualified Zlib

png :: BS.ByteString
png = unsafePerformIO $ BS.hGetContents =<< openBinaryFile "../small.png" ReadMode

decode :: BS.ByteString -> Maybe PNG
decode bs = do
	(mh, d) <-
		(fromChunk *** BS.concat . map dat . filter ((== "IDAT") . typ))
			<$> (uncons =<< fromPng bs)
	h <- mh
	(z, d') <- runStateT Zlib.header d
	return $ PNG h z d'

encode :: PNG -> BS.ByteString
encode p = toPng [
	toChunk $ header p,
	Chunk "IDAT" $ Zlib.encodeHeader (zlib p) `BS.append` body p
	]

data PNG = PNG {
	header :: IHDR,
	zlib :: (Zlib.Cmf, Zlib.FLvl, Maybe Zlib.FDct),
	body :: BS.ByteString
	} deriving Show

uncons :: [a] -> Maybe (a, [a])
uncons (x : xs) = Just (x, xs)
uncons _ = Nothing

adler32 :: BS.ByteString -> BS.ByteString
adler32 = BS.pack
	. (\((a1, a0), (b1, b0)) -> [b1, b0, a1, a0])
	. ((fromIntegral *** fromIntegral) . (`divMod` 0x100)
		*** (fromIntegral *** fromIntegral) . (`divMod` 0x100))
	. BS.foldl' (\(a, b) x ->
		let a' = (a + fromIntegral x) `mod` 65521 in (a', (b + a') `mod` 65521)) (1, 0)
	. BS.map fromIntegral

noCompress :: BS.ByteString -> BS.ByteString
noCompress bs =
	"\x01" `BS.append` l `BS.append` comp l `BS.append` bs `BS.append` adler32 bs
	where
	l = litEnd 2 $ BS.length bs

litEnd :: Int -> Int -> BS.ByteString
litEnd 0 _ = ""
litEnd c n = fromIntegral n `BS.cons` litEnd (c - 1) (n `shiftR` 8)

comp :: BS.ByteString -> BS.ByteString
comp = BS.map complement

sample :: PNG
sample = PNG {
	header = sampleIHDR,
	zlib = (Zlib.Deflate 32768, Zlib.FLvl 0, Nothing),
	body = noCompress $
		"\x00\xff\xff\xff\xff\xff" `BS.append`
		"\x00\xff\xff\x00\xff\xff" `BS.append`
		"\x00\xff\x00\x00\x00\xff" `BS.append`
		"\x00\xff\xff\x00\xff\xff" `BS.append`
		"\x00\xff\xff\xff\xff\xff"
	}

sampleIHDR :: IHDR
sampleIHDR = IHDR {
	width = 5,
	height = 5,
	bitDepth = 8,
	colorType = NoPalette Grayscale NoAlpha,
	compressionMethod = 0,
	filterMethod = 0,
	interlaceMethod = 0
	}
