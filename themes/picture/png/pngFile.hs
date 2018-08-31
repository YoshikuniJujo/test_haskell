{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.State
import Data.Word
import System.IO.Unsafe
import Codec.Compression.Zlib

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

sample :: BS.ByteString
sample = unsafePerformIO $ BS.readFile "png-files/tux_mono.png"

type BinaryM = StateT BS.ByteString (Either String)

headerBytes :: BS.ByteString
headerBytes = "\x89PNG\x0D\x0A\x1A\x0A"

takeBytes :: Int -> BinaryM BS.ByteString
takeBytes = state . BS.splitAt

oneByte :: BinaryM Word8
oneByte = do
	s <- get
	case BS.uncons s of
		Just (b, s') -> b <$ put s'
		Nothing -> fail "no more bytes"

header :: BinaryM ()
header = do
	h <- takeBytes 8
	guard $ h == headerBytes

chankHeader :: BinaryM (Int, BS.ByteString)
chankHeader = do
	cs <- takeBytes 4
	cn <- takeBytes 4
	return (readInt cs, cn)

chank :: BinaryM (BS.ByteString, BS.ByteString)
chank = do
	(n, cn) <- chankHeader
	cb <- takeBytes n
	_crc <- takeBytes 4
	return (cn, cb)

readInt :: BS.ByteString -> Int
readInt = bigEndian 0 . BS.unpack

bigEndian :: Int -> [Word8] -> Int
bigEndian s (w : ws) = bigEndian (s * 2 ^ (8 :: Int) + fromIntegral w) ws
bigEndian s [] = s

data Ihdr = Ihdr {
	ihdrWidth :: Int,
	ihdrHeight :: Int,
	ihdrDepth :: Word8,
	ihdrColorType :: Word8,
	ihdrCompMethod :: Word8,
	ihdrFilterMethod :: Word8,
	ihdrILaceMethod :: Word8
	} deriving Show

toIhdr :: BinaryM Ihdr
toIhdr = Ihdr
	<$> (readInt <$> takeBytes 4)
	<*> (readInt <$> takeBytes 4)
	<*> oneByte <*> oneByte <*> oneByte <*> oneByte <*> oneByte

ihdr :: BinaryM Ihdr
ihdr = do
	("IHDR", i) <- chank
	case runStateT toIhdr i of
		Right (ih, _) -> return ih
		Left e -> fail e

chankIdat :: BinaryM BS.ByteString
chankIdat = do
	(cn, cb) <- chank
	case cn of
		"IDAT" -> return cb
		_ -> chankIdat

getIdat :: BinaryM (Ihdr, BS.ByteString)
getIdat = header >> (,) <$> ihdr <*> chankIdat

decompressed :: BinaryM (Ihdr, LBS.ByteString)
decompressed = do
	(h, d) <- getIdat
	case decompress $ LBS.fromStrict d of
		Right dc -> return (h, dc)
		Left e -> fail $ show e

unfiltered :: BinaryM BS.ByteString
unfiltered = do
	(hd, dc) <- decompressed
	let	w = ihdrWidth hd
		h = ihdrHeight hd
	guard $ ihdrDepth hd == 8
	case runStateT (unfilter w h) $ LBS.toStrict dc of
		Right (r, _) -> return r
		Left e -> fail e

unfilter :: Int -> Int -> BinaryM BS.ByteString
unfilter w h = do
	takeBytes (w + 1)

unfilterLine :: Word8 -> BS.ByteString -> BS.ByteString
unfilterLine 0 bs = bs
-- unfilterLine 1 bs =
