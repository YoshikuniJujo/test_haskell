{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckWav (wavData1, wavData2) where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.Bits
import Data.Word
import Data.Int
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.IO.Unsafe

sample1, sample2 :: BS.ByteString
sample1 = unsafePerformIO $ BS.readFile "Noise.wav"
sample2 = unsafePerformIO $ BS.readFile "Front_Center.wav"

type WavM = StateT BS.ByteString Maybe

data Wav = Wav {
	chunkId :: BS.ByteString,
	chunkSize :: Word32,
	format :: BS.ByteString,
	subchunk1Id :: BS.ByteString,
	subchunk1Size :: Word32,
	audioFormat :: AudioFormat,
	numChannels :: Word16,
	sampleRate :: Word32,
	byteRate :: Word32,
	blockAlign :: Word16,
	bitsPerSample :: Word16,
	subchunk2Id :: BS.ByteString,
	subchunk2Size :: Word32,
	rest :: BS.ByteString }

instance Show Wav where
	show w = "(Wav { " ++
		"chunkId = " ++ show (chunkId w) ++ ", " ++
		"chunkSize = " ++ show (chunkSize w) ++ ", " ++
		"subchunk1Id = " ++ show (subchunk1Id w) ++ ", " ++
		"subchunk1Size = " ++ show (subchunk1Size w) ++ ", " ++
		"audioFormat = " ++ show (audioFormat w) ++ ", " ++
		"numChannels = " ++ show (numChannels w) ++ ", " ++
		"sampleRate = " ++ show (sampleRate w) ++ ", " ++
		"byteRate = " ++ show (byteRate w) ++ ", " ++
		"blockAlign = " ++ show (blockAlign w) ++ ", " ++
		"bitsPerSample = " ++ show (bitsPerSample w) ++ ", " ++
		"subchunk2Id = " ++ show (subchunk2Id w) ++ ", " ++
		"subchunk2Size = " ++ show (subchunk2Size w) ++ ", " ++
		"... }"

data AudioFormat = Pcm | OtherAudioFormat Word16 deriving Show

readAudioFormat :: Word16 -> AudioFormat
readAudioFormat = \case 1 -> Pcm; n -> OtherAudioFormat n

toStateJust :: (s -> (a, s)) -> StateT s Maybe a
toStateJust = StateT . (Just .)

bytes :: Int -> WavM BS.ByteString
bytes n = StateT $ \case "" -> Nothing; src -> Just $ BS.splitAt n src

littleNum :: (Bits a, Num a) => BS.ByteString -> a
littleNum = ln . BS.unpack
	where
	ln [] = 0
	ln (w : ws) = fromIntegral w + ln ws `shift` 8

getChunkId :: WavM BS.ByteString
getChunkId = bytes 4

getChunkSize :: WavM Word32
getChunkSize = littleNum <$> bytes 4

getFormat :: WavM BS.ByteString
getFormat = bytes 4

getSubchunk1Id :: WavM BS.ByteString
getSubchunk1Id = bytes 4

getSubchunk1Size :: WavM Word32
getSubchunk1Size = littleNum <$> bytes 4

getAudioFormat :: WavM AudioFormat
getAudioFormat = readAudioFormat . littleNum <$> bytes 2

getNumChannels :: WavM Word16
getNumChannels = littleNum <$> bytes 2

getSampleRate :: WavM Word32
getSampleRate = littleNum <$> bytes 4

getByteRate :: WavM Word32
getByteRate = littleNum <$> bytes 4

getBlockAlign :: WavM Word16
getBlockAlign = littleNum <$> bytes 2

getBitsPerSample :: WavM Word16
getBitsPerSample = littleNum <$> bytes 2

getSubchunk2Id :: WavM BS.ByteString
getSubchunk2Id = bytes 4

getSubchunk2Size :: WavM Word32
getSubchunk2Size = littleNum <$> bytes 4

analyze :: BS.ByteString -> Maybe Wav
analyze src = ((uncurry ($)) <$>) . (`runStateT` src) $ Wav
	<$> getChunkId
	<*> getChunkSize
	<*> getFormat
	<*> getSubchunk1Id
	<*> getSubchunk1Size
	<*> getAudioFormat
	<*> getNumChannels
	<*> getSampleRate
	<*> getByteRate
	<*> getBlockAlign
	<*> getBitsPerSample
	<*> getSubchunk2Id
	<*> getSubchunk2Size

catchMaybe :: WavM a -> WavM (Maybe a)
catchMaybe act = StateT $ \s -> case runStateT act s of
	Just (x, s) -> Just (Just x, s)
	Nothing -> Just (Nothing, s)

doWhile :: Monad m => m (Maybe a) -> m [a]
doWhile act = do
	mr <- act
	case mr of
		Just r -> (r :) <$> doWhile act
		Nothing -> return []

analyzeData :: WavM [Int16]
analyzeData = doWhile . catchMaybe $ littleNum <$> bytes 2

wavData1 :: [Int16]
wavData1 = fromJust
	$ fst <$> analyzeData `runStateT` rest (fromJust $ analyze sample1)

wavData2 :: [Int16]
wavData2 = fromJust
	$ fst <$> analyzeData `runStateT` rest (fromJust $ analyze sample2)
