{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Lifegame.Board (

	-- * BOARD DATA TYPE AND GENERATIONS

	B, generations,

	-- * CONVERSION BETWEEN BOARD AND GRAY1

	toGray1,

	-- * PUT SHAPE

	putShapeAscii,

	-- * PRINT AS ASCII

	printAsAscii

	) where

import Prelude hiding (read)
import GHC.Generics
import Control.DeepSeq
import Data.Bits
import Data.Vector qualified as V
import Data.Bool
import Data.Word
import Data.Image.Gray1 qualified as Gray1

-- BOARD DATA TYPE

data B = B { width :: Int, height :: Int, body :: V.Vector Word8 }
	deriving (Generic, Eq, Show)

instance NFData B

read :: B -> Int -> Int -> Bool
read B { width = w, height = h, body = bd } x y
	| x < 0 || w <= x || y < 0 || h <= y =  False
	| otherwise = testBit (bd V.! (w' * y + (x `div` 8))) $ 7 - x `mod` 8
	where w' = w `div'` 8

generate :: Int -> Int -> (Int -> Int -> Bool) -> B
generate w h px = B { width = w, height = h, body = generateBody w h px }

generateBody :: Int -> Int -> (Int -> Int -> Bool) -> V.Vector Word8
generateBody w h px = V.generate (w' * h) \i ->
	boolsToWord $ (<$> [0 .. 7]) \x -> px' (i `mod` w' * 8 + x) (i `div` w')
	where
	px' x y	| x >= w = False | otherwise = px x y
	w' = w `div'` 8

boolsToWord :: [Bool] -> Word8
boolsToWord bls = go 0 $ bls ++ replicate (8 - length bls) False
	where go r = \case
		[] -> r; b : bs -> go (bool id (`setBit` 0) b $ r `shiftL` 1) bs

-- GENERATIONS

generations :: B -> [B]
generations = iterate next

next :: B -> B
next bd = generate (width bd) (height bd) (calc bd)

calc :: B -> Int -> Int -> Bool
calc p x y
	| not l && ns == 3 = True | l && 2 <= ns && ns <= 3 = True
	| otherwise = False
	where
	l = read p x y
	ns = length . filter id $ (uncurry $ read p) <$> [ (z, w) |
		z <- [x - 1 .. x + 1], w <- [y - 1 .. y + 1], (z, w) /= (x, y) ]

-- CONVERSION BETWEEN BOARD AND GRAY1

toGray1 :: B -> Gray1.G
toGray1 B { width = w, height = h, body = bd } =
	Gray1.G { Gray1.width = w, Gray1.height = h, Gray1.body = bd }

-- PUT/ADD SHAPE

putShapeAscii :: Int -> Int -> Int -> Int -> [String] -> B
putShapeAscii w h xo yo = putShape w h xo yo . (((== '*') <$>) <$>)

putShape :: Int -> Int -> Int -> Int -> [[Bool]] -> B
putShape w h xo yo = generate w h . putShapePixel xo yo

putShapePixel :: Int -> Int -> [[Bool]] -> Int -> Int -> Bool
putShapePixel xo yo bss x y
	|	xo <= x && x < xo + (length $ head bss) &&
		yo <= y && y < yo + (length bss) = bss !! (y - yo) !! (x - xo)
	| otherwise = False

div' :: Integral n => n -> n -> n
a `div'` b = (a - 1) `div` b + 1

printAsAscii :: B -> IO ()
printAsAscii = (putStrLn `mapM_`) . toAscii

toAscii :: B -> [String]
toAscii = (((bool '.' '*') <$>) <$>) . toBools

toBools :: B -> [[Bool]]
toBools b@B { width = w, height = h } = (\y -> (\x -> read b x y) <$> [0 .. w - 1]) <$> [0 .. h - 1]
