{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Word
import Data.Int
import Data.ByteString qualified as BS
import System.Environment

import Data.JuicyCairo
import Codec.Picture
import SampleImages

import Waveform
import Poppable

main :: IO ()
main = do
	fp : wav : (read -> dx) : (read -> dy) : (read -> dr) : as <- getArgs
	let	tk = case as of
			[a] -> Just $ read a
			[] -> Nothing
			_ -> error "bad arguments"
--	ewv <- stripRiff <$> BS.readFile "/home/tatsuya/tmp/aaaaa.wav"
	ewv <- stripRiff <$> BS.readFile wav
--	either error (\wv -> checkStriped wv 20000 Nothing 0.0068 0.1) ewv
	either error (\wv -> checkStriped fp wv dr tk dx dy) ewv

checkStriped :: FilePath -> BS.ByteString -> Int -> Maybe Int -> Double -> Double -> IO ()
checkStriped fp bs dr tk dx dy = do
	let	(fmt, chs) = BS.splitAt 4 bs
		(fch, r1) = chunk chs
		ch = channels $ fst . pop @WaveFormatEx <$> fch
		Chunk { chunkPayload = (l, r) } = case ch of
			1 -> (readData' <$>) . fst $ chunk r1
			2 -> (readData <$>) . fst $ chunk r1
			_ -> error "not yet implemented"
	print fmt
	print $ pop @WaveFormatEx <$> fch
--	printSome . fst $ chunk r1
--	print . (readData <$>) . fst $ chunk r1
	print l
	print r
	print . channels $ fst . pop @WaveFormatEx <$> fch
	writePng fp . cairoArgb32ToJuicyRGBA8
		. simpleGlaph 1536 768 10 384 dx dy
		. maybe id take tk $ drop dr l
	print $ pop @WaveFormatEx <$> fch
	when (ch == 1) $ printMonoral16 Monoral16 {
		waveFormat = chunkPayload $ fst . pop <$> fch,
		waveData = l }

readData :: BS.ByteString -> ([Int16], [Int16])
readData bs
	| BS.null bs = ([], [])
	| otherwise = let
		(l, r1) = pop bs
		(r, r2) = pop r1 in (l :) *** (r :) $ readData r2

readData' :: BS.ByteString -> ([Int16], [Int16])
readData' bs
	| BS.null bs = ([], [])
	| otherwise = let
		(l, r1) = pop bs in (l :) `first` readData' r1

stripRiff :: BS.ByteString -> Either String BS.ByteString
stripRiff bs = case BS.splitAt 4 bs of
	("RIFF", r1) -> let
		(sz, r2) = first (
				foldr (\x -> (fromIntegral x .|.) . (`shiftL` 8)
				) 0 . BS.unpack )
			$ BS.splitAt 4 r1
		(pl, _r3) = BS.splitAt sz r2 in
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

channels :: Chunk WaveFormatEx -> Word16
channels Chunk { chunkPayload = wf } = nChannels wf

instance Functor Chunk where
	f `fmap` Chunk { chunkFourCC = fcc, chunkPayload = x } = Chunk {
		chunkFourCC = fcc,
		chunkPayload = f x }
