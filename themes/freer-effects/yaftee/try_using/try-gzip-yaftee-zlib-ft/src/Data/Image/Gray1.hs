{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Gray1 where

import Data.Bits
import Data.Vector qualified as V
import Data.Bool
import Data.Word

data G = G { width :: Int, height :: Int, body :: V.Vector Word8 } deriving Show

rows :: G -> [[Word8]]
rows G { width = w, height = h, body = bd } =
	(<$> [0 .. h - 1]) \y -> (<$> [0 .. (w - 1) `div` 8]) \x ->
		bd V.! (y * ((w - 1) `div` 8 + 1) + x)

generate :: Int -> Int -> (Int -> Int -> Bool) -> G
generate w h px = G {
	width = w, height = h,
	body = V.generate (w' * h) \i ->
		boolsToWord $ (<$> [0 .. 7]) \dx ->
			px' (i `mod` w' * 8 + dx) (i `div` w') }
	where
	px' x y	| x >= w = False
		| otherwise = px x y
	w' = (w - 1) `div` 8 + 1

boolsToWord :: [Bool] -> Word8
boolsToWord bs = go 0 bs'
	where
	go r [] = r
	go r (b : bs) = go (bool id (`setBit` 0) b $ r `shiftL` 1) bs
	bs' = bs ++ replicate (8 - length bs) False

printAsAscii :: G -> IO ()
printAsAscii = (putStrLn `mapM_`) . toAscii

toAscii :: G -> [String]
toAscii g = (take (width g) . concat . (wordToAscii <$>) <$>) $ rows g

wordToAscii :: Word8 -> String
wordToAscii w = bool '.' '*' . testBit w <$> [7, 6 .. 0]

sample0, sample1, sample2 :: G
sample0 = G {
	width = 16, height = 5,
	body = V.generate (2 * 5) fromIntegral }

sample1 = G {
	width = 15, height = 5,
	body = V.generate (2 * 5) fromIntegral }

sample2 = G {
	width = 17, height = 5,
	body = V.generate (3 * 5) fromIntegral }

fromAscii :: [String] -> G
fromAscii asc = generate w h (\x y -> asc !! y !! x == '*')
	where
	w = length $ head asc
	h = length asc

sep :: Int -> [a] -> [[a]]
sep _ [] = []
sep n xs = take n xs : sep n (drop n xs)
