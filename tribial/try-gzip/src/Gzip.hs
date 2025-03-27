{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gzip where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS

import Crc

type MyMonad = StateT BS.ByteString (ExceptT String IO)

runMyMonad ::
	BS.ByteString -> MyMonad a -> IO (Either String (a, BS.ByteString))
runMyMonad bs = runExceptT . (`runStateT` bs)

pop :: MyMonad Word8
pop = do
	bs <- get
	case BS.uncons bs of
		Nothing -> throwError "empty"
		Just (b, bs') -> b <$ put bs'

takeBytes :: Int -> MyMonad BS.ByteString
takeBytes n = do
	bs <- get
	when (BS.length bs < n) $ throwError "not enough"
	BS.take n bs <$ put (BS.drop n bs)

takeWord32 :: MyMonad Word32
takeWord32 = bsToNum <$> takeBytes 4

takeString :: MyMonad BS.ByteString
takeString = do
	bs <- get
	let	(str, bs') = BS.span (/= 0) bs
	str <$ put bs' <* pop

print' :: Show a => a -> MyMonad ()
print' = liftIO . print

bsToNum :: (Bits n, Integral n) => BS.ByteString -> n
bsToNum = foldr (\b s -> fromIntegral b .|. s `shiftL` 8) 0 . BS.unpack

numToBs :: (Bits n, Integral n) => n -> BS.ByteString
numToBs 0 = ""
numToBs n = fromIntegral (n .&. 0xff) `BS.cons` numToBs (n `shiftR` 8)

bits :: Word8 -> [Bool]
bits b = (b `testBit`) <$> [0 .. 7]

ids0 :: BS.ByteString
ids0 = "\x1f\x8b"

data GzipHeader = GzipHeader {
	gzipHeaderCompressionMethod :: Word8,
	gzipHeaderFlags :: Flags,
	gzipHeaderModificationTime :: Word32,
	gzipExtraFlags :: Word8,
	gzipOperatingSystem :: Word8,
	gzipFileName :: BS.ByteString }
	deriving Show

readFlags :: Word8 -> Maybe Flags
readFlags w = if or $ (w `testBit`) <$> [5 .. 7]
	then Nothing
	else Just Flags {
		flagsText = w `testBit` 0,
		flagsHcrc = w `testBit` 1,
		flagsExtra = w `testBit` 2,
		flagsName = w `testBit` 3,
		flagsComment = w `testBit` 4 }

encodeFlags :: Flags -> Word8
encodeFlags fs = foldl setBit 0 xs
	where
	xs = map fst . filter snd . zip [0 ..] $ ($ fs) <$>
		[flagsText, flagsHcrc, flagsExtra, flagsName, flagsComment]

data Flags = Flags {
	flagsText :: Bool,
	flagsHcrc :: Bool,
	flagsExtra :: Bool,
	flagsName :: Bool,
	flagsComment :: Bool }
	deriving Show

encodeGzipHeader :: GzipHeader -> BS.ByteString
encodeGzipHeader hdr = ids0 `BS.append`
	(gzipHeaderCompressionMethod hdr `BS.cons`
		encodeFlags (gzipHeaderFlags hdr) `BS.cons`
		numToBs (gzipHeaderModificationTime hdr)) `BS.append`
	(gzipExtraFlags hdr `BS.cons`
		gzipOperatingSystem hdr `BS.cons`
		gzipFileName hdr) `BS.snoc`
	0

sampleGzipHeader :: GzipHeader
sampleGzipHeader = GzipHeader {
	gzipHeaderCompressionMethod = 8,
	gzipHeaderFlags = Flags {
		flagsText = False,
		flagsHcrc = False,
		flagsExtra = False,
		flagsName = True,
		flagsComment = False },
	gzipHeaderModificationTime = 1743055415,
	gzipExtraFlags = 0,
	gzipOperatingSystem = 3,
	gzipFileName = "abcd.txt" }

crc' :: BS.ByteString -> BS.ByteString
crc' = numToBs . crc
