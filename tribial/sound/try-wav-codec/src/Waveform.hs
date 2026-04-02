{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Waveform (
	Monoral16(..), printMonoral16, writeWave,
	WaveFormatEx(..), sampleWaveFormatEx,
	FormatTag(..),
	pattern WaveFormatPcm, pattern WaveFormatIeeeFloat, pattern WaveFormatMulaw
	) where

import Foreign.C.Enum
import Data.Word
import Data.Int
import Data.ByteString qualified as BS

import Data.JuicyCairo
import Codec.Picture
import SampleImages

import Poppable

enum "FormatTag" ''Word16 [''Show, ''Read, ''Eq] [
	("WaveFormatPcm", 1),
	("WaveFormatIeeeFloat", 3),
	("WaveFormatMulaw", 7) ]

writeWave :: FilePath -> Monoral16 -> Int -> Maybe Int -> Double -> Double -> IO ()
writeWave fp Monoral16 { waveData = wv } dr tk dx dy =
	writePng fp . cairoArgb32ToJuicyRGBA8
		. simpleGlaph 1536 768 10 384 dx dy
		. maybe id take tk $ drop dr wv

data Chunk a = Chunk {
	chunkFourCC :: BS.ByteString,
	chunkPayload :: a }
	deriving Show

instance Functor Chunk where
	f `fmap` Chunk { chunkFourCC = fcc, chunkPayload = x } = Chunk {
		chunkFourCC = fcc,
		chunkPayload = f x }

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
		(bps, _r6) = pop r5
--		(cbs, r7) = pop r6
		in WaveFormatEx {
		wFormatTag = ft,
		nChannels = cnns,
		nSamplePerSec = sps,
		nAvgBytesPerSec = abps,
		nBlockAlign = balgn,
		wBitsPerSample = bps }
--		cbSize = cbs }

data Monoral16 = Monoral16 {
	waveFormat :: WaveFormatEx,
	waveData :: [Int16] }
	deriving Show

printMonoral16 :: Monoral16 -> IO ()
printMonoral16 = putStrLn . showMonoral16

showMonoral16 :: Monoral16 -> String
showMonoral16 m =
	"(Monoral16 { waveFormat = " ++ show (waveFormat m) ++ ", waveData = " ++ take 200 (show $ waveData m) ++ "... }"

sampleWaveFormatEx :: WaveFormatEx
sampleWaveFormatEx = WaveFormatEx {
	wFormatTag = WaveFormatPcm,
	nChannels = 1,
	nSamplePerSec = 48000,
	nAvgBytesPerSec = 96000,
	nBlockAlign = 2,
	wBitsPerSample = 16 }
