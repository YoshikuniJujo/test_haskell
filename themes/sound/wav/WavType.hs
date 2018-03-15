{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module WavType (
	Wav, readWav, fromWav, wavData,
	AudioFormat, readAudioFormat, fromAudioFormat,
	Err(..),
	sliceWav, lengthWav ) where

import Data.Bool (bool)
import Data.Word (Word16, Word32)
import Data.Int (Int16)

import qualified Data.ByteString as BS

data Err = Eof | Err String deriving Show

data Wav = Wav {
	numChannels :: Word16,
	sampleRate :: Word32,
	bitsPerSample :: Word16,
	wavData :: [Int16] }

instance Show Wav where
	show w = "(Wav { " ++
		"numChannels = " ++ show (numChannels w) ++ ", " ++
		"sampleRate = " ++ show (sampleRate w) ++ ", " ++
		"bitsPerSample = " ++ show (bitsPerSample w) ++ ", " ++
		"wavData = " ++ init (show . take 256 $ wavData w) ++
		"...] }"

check :: Bool -> String -> Either Err ()
check b e = bool (Left $ Err e) (return ()) b

readWav :: BS.ByteString -> Word32 -> BS.ByteString -> BS.ByteString ->
	Word32 -> AudioFormat -> Word16 -> Word32 -> Word32 -> Word16 ->
	Word16 -> BS.ByteString -> [Int16] -> Either Err Wav
readWav cid cs f sc1id sc1s af nc sr br ba bps sc2id r = do
	check (cid == "RIFF") "readWav: ChunkId should be `RIFF'"
	check (cs == sc2s bps + 36) $ "readWav: ChunkSize should be " ++
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
	where sc2s = (fromIntegral (length r) *) . (`div` 8) . fromIntegral

fromWav :: Wav -> (
	BS.ByteString, Word32, BS.ByteString, BS.ByteString,
	Word32, AudioFormat, Word16, Word32, Word32, Word16,
	Word16, BS.ByteString, [Int16])
fromWav w = (
	"RIFF", sc2s + 36, "WAVE", "fmt ",
	16, Pcm, nc, sr, sr * fromIntegral (nc * bps `div` 8),
	nc * bps `div` 8,
	bps, "data", r )
	where
	sc2s = fromIntegral (length r) * fromIntegral bps `div` 8
	nc = numChannels w
	sr = sampleRate w
	bps = bitsPerSample w
	r = wavData w

data AudioFormat = Pcm | OtherAudioFormat Word16 deriving (Show, Eq)

readAudioFormat :: Word16 -> AudioFormat
readAudioFormat = \case 1 -> Pcm; n -> OtherAudioFormat n

fromAudioFormat :: AudioFormat -> Word16
fromAudioFormat = \case
	Pcm -> 1
	OtherAudioFormat n -> n

sliceWav :: Wav -> Int -> Int -> Wav
sliceWav w b e = w { wavData = take (e - b) . drop b $ wavData w }

lengthWav :: Wav -> Int
lengthWav = length . wavData
