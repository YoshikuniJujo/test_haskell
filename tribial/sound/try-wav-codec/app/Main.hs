{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Enum
import Control.Arrow
import Data.Bits
import Data.Word
import Data.Int
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

enum "FormatTag" ''Word16 [''Show, ''Read, ''Eq] [
	("WaveFormatPcm", 1),
	("WaveFormatIeeeFloat", 3),
	("WaveFormatMulaw", 7) ]

main :: IO ()
main = do
	ewv <- stripRiff <$> BS.readFile "/home/tatsuya/tmp/aaaaa.wav"
	either error checkStriped ewv

checkStriped :: BS.ByteString -> IO ()
checkStriped bs = do
	let	(fmt, chs) = BS.splitAt 4 bs
		(fch, r1) = chunk chs
	print fmt
	print $ pop @WaveFormatEx <$> fch
--	printSome . fst $ chunk r1
	print . (readData <$>) . fst $ chunk r1

readData :: BS.ByteString -> ([Int16], [Int16])
readData bs
	| BS.null bs = ([], [])
	| otherwise = let
		(l, r1) = pop bs
		(r, r2) = pop r1 in (l :) *** (r :) $ readData r2

stripRiff :: BS.ByteString -> Either String BS.ByteString
stripRiff bs = case BS.splitAt 4 bs of
	("RIFF", r1) -> let
		(sz, r2) = first (
				foldr (\x -> (fromIntegral x .|.) . (`shiftL` 8)
				) 0 . BS.unpack )
			$ BS.splitAt 4 r1
		(pl, r3) = BS.splitAt sz r2 in
		Right pl
	_ -> Left "no RIFF magic number"

chunk :: BS.ByteString -> (Chunk BS.ByteString, BS.ByteString)
chunk bs = let
	(fcc, r1) = BS.splitAt 4 bs
	(sz, r2) = first (
			foldr (\x -> (fromIntegral x .|.) . (`shiftL` 8)
			) 0 . BS.unpack )
		$ BS.splitAt 4 r1
	(pl, r3) = BS.splitAt sz r2 in
	(Chunk fcc pl, r3)

data Chunk a = Chunk {
	chunkFourCC :: BS.ByteString,
	chunkPayload :: a }
	deriving Show

instance Functor Chunk where
	f `fmap` Chunk { chunkFourCC = fcc, chunkPayload = x } = Chunk {
		chunkFourCC = fcc,
		chunkPayload = f x }

printSome :: Show a => Chunk a -> IO ()
printSome = putStrLn . showSome

showSome :: Show a => Chunk a -> String
showSome Chunk { chunkFourCC = fcc, chunkPayload = pl } =
	"(Chunk " ++ BSC.unpack fcc ++ " " ++ take 100 (show pl) ++ "...)"

pop :: forall a . Poppable a => BS.ByteString -> (a, BS.ByteString)
pop bs = fromByteString `first` (BS.splitAt (byteLength @a) bs)

class Poppable a where
	byteLength :: Int
	fromByteString :: BS.ByteString -> a

instance Poppable Word16 where
	byteLength = 2
	fromByteString bs = let
		[a, b] = fromIntegral <$> BS.unpack (BS.take 2 bs) in
		a .|. b `shiftL` 8

instance Poppable Int16 where
	byteLength = 2
	fromByteString bs = let
		[a, b] = fromIntegral <$> BS.unpack (BS.take 2 bs) in
		a .|. b `shiftL` 8

instance Poppable Word32 where
	byteLength = 4
	fromByteString =
		foldr (\x -> (fromIntegral x .|.) . (`shiftL` 8)) 0 . BS.unpack

instance Poppable FormatTag where
	byteLength = 2
	fromByteString = FormatTag . fromByteString

data WaveFormatEx = WaveFormatEx {
	wFormatTag :: FormatTag,
	nChannels :: Word16,
	nSamplePerSec :: Word32,
	nAvgBytesPerSec :: Word32,
	nBlockAlign :: Word16,
	wBitsPerSample :: Word16 }
--	cbSize :: Word16 }
	deriving Show

instance Poppable WaveFormatEx where
	byteLength = 16
	fromByteString bs = let
		(ft, r1) = pop bs
		(cnns, r2) = pop r1
		(sps, r3) = pop r2
		(abps, r4) = pop r3
		(balgn, r5) = pop r4
		(bps, r6) = pop r5
--		(cbs, r7) = pop r6
		in WaveFormatEx {
		wFormatTag = ft,
		nChannels = cnns,
		nSamplePerSec = sps,
		nAvgBytesPerSec = abps,
		nBlockAlign = balgn,
		wBitsPerSample = bps }
--		cbSize = cbs }
