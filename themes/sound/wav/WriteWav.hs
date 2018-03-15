{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module WriteWav (Wav, writeWav) where

import Data.Monoid
import Data.Bits
import Data.Int

import qualified Data.ByteString as BS

import WavType

writeWav :: Wav -> BS.ByteString
writeWav w = cid <> fromNumLittle 4 cs <> f <> sc1id <> fromNumLittle 4 sc1s <>
	fromNumLittle 2 (fromAudioFormat af) <> fromNumLittle 2 nc <>
	fromNumLittle 4 sr <> fromNumLittle 4 br <> fromNumLittle 2 ba <>
	fromNumLittle 2 bps <> sc2id <> fromNumLittle 4 (BS.length dt) <> dt
	where
	(cid, cs, f, sc1id, sc1s, af, nc, sr, br, ba, bps, sc2id, dt_) =
		fromWav w
	dt = readData dt_

fromNumLittle :: (Bits a, Integral a) => Int -> a -> BS.ByteString
fromNumLittle n _ | n <= 0 = ""
fromNumLittle n x = fromIntegral x `BS.cons` fromNumLittle (n - 1) (x `shiftR` 8)

readData :: [Int16] -> BS.ByteString
readData = BS.concat . map (fromNumLittle 2)
