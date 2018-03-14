{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckWav (wavData1, wavData2) where

import Control.Monad.State
import Data.Bool
import Data.Either
import Data.Bits
import Data.Word
import Data.Int
import qualified Data.ByteString as BS
import System.IO.Unsafe

sample1, sample2 :: BS.ByteString
sample1 = unsafePerformIO $ BS.readFile "Noise.wav"
sample2 = unsafePerformIO $ BS.readFile "Front_Center.wav"

type WavM = StateT BS.ByteString (Either Err)

data Err = Eof | Err String deriving Show

data Wav = Wav {
	numChannels :: Word16,
	sampleRate :: Word32,
	bitsPerSample :: Word16,
	wavData :: BS.ByteString }

instance Show Wav where
	show w = "(Wav { " ++
		"numChannels = " ++ show (numChannels w) ++ ", " ++
		"sampleRate = " ++ show (sampleRate w) ++ ", " ++
		"bitsPerSample = " ++ show (bitsPerSample w) ++ ", " ++
		"wavData = " ++ init (show . BS.take 256 $ wavData w) ++
		"...\" }"

check :: Bool -> String -> Either Err ()
check b e = bool (Left $ Err e) (return ()) b

readWav :: BS.ByteString -> Word32 -> BS.ByteString -> BS.ByteString ->
	Word32 -> AudioFormat -> Word16 -> Word32 -> Word32 -> Word16 ->
	Word16 -> BS.ByteString -> BS.ByteString -> Either Err Wav
readWav cid cs f sc1id sc1s af nc sr br ba bps sc2id r = do
	check (cid == "RIFF") "readWav: ChunkId should be `RIFF'"
	check (cs == sc2s + 36) $ "readWav: ChunkSize should be " ++
		"Subchunk2Size + 36"
	check (f == "WAVE") "readWav: Format should be `WAVE'"
	check (sc1id == "fmt ") "readWav: Subchunk1Id should be `fmt '"
	check (sc1s == 16) "readWav: Subchunk1Size should be 16"
	check (af == Pcm) "readWav: AudioFormat should be 1 (PCM)"
	check (br == sr * fromIntegral (nc * bps `div` 8)) $
		"readWav ByteRate should be " ++
			"SampleRate * NumChannels * BitsPerSample / 8"
	check (ba == nc * bps `div` 8)
		"readWav BlockAlign should be NumChannels * BitsPerSample / 8"
	check (sc2id == "data") "readWav: Subchunk2Id should be `data'"
	return $ Wav nc sr bps r
	where
	sc2s = fromIntegral $ BS.length r

data AudioFormat = Pcm | OtherAudioFormat Word16 deriving (Show, Eq)

readAudioFormat :: Word16 -> AudioFormat
readAudioFormat = \case 1 -> Pcm; n -> OtherAudioFormat n

bytes :: Int -> WavM BS.ByteString
bytes n = StateT $ \case
	"" -> Left Eof
	src -> Right $ BS.splitAt n src

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

analyze :: BS.ByteString -> Either Err Wav
analyze src = join . (`evalStateT` src) $ readWav
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
	<*> (getSubchunk2Size >>= bytes . fromIntegral)

catchEof :: WavM a -> WavM (Maybe a)
catchEof act = StateT $ \s -> case runStateT act s of
	Right (x, s') -> Right (Just x, s')
	Left Eof -> Right (Nothing, s)
	Left e -> Left e

doWhile :: Monad m => m (Maybe a) -> m [a]
doWhile act = do
	mr <- act
	case mr of
		Just r -> (r :) <$> doWhile act
		Nothing -> return []

analyzeData :: WavM [Int16]
analyzeData = doWhile . catchEof $ littleNum <$> bytes 2

wavData1 :: [Int16]
wavData1 = fromRight []
	$ fst <$> analyzeData `runStateT` wavData (fromRight undefined $ analyze sample1)

wavData2 :: [Int16]
wavData2 = fromRight []
	$ fst <$> analyzeData `runStateT` wavData (fromRight  undefined $ analyze sample2)
