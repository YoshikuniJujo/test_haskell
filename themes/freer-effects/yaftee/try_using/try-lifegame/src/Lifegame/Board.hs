{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Lifegame.Board (

	-- * BOARD DATA TYPE

	B, height, checkTopEdge, checkBottomEdge,

	-- * GENERATIONS

	generations, next,

	-- * CONVERSION BETWEEN BOARD AND GRAY1

	toGray1, enlargeToGray1, fromGray1,

	-- * PUT/ADD SHAPE

	putShapeAscii, addShapeAscii,

	-- * MATCHING

	multiMatchLivesTop, multiMatchLivesBottom, Pattern, asciiToPattern,

	-- * CLEAR

	clear, Area(..)

	) where

import Prelude hiding (read)
import GHC.Generics
import Control.Arrow
import Control.DeepSeq
import Data.Bits
import Data.Maybe
import Data.Either
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Bool
import Data.Word
import Data.Image.Gray1 qualified as Gray1

-- BOARD DATA TYPE

data B = B { width :: Int, height :: Int, body :: V.Vector Word8 }
	deriving (Generic, Eq, Show)

instance NFData B

read :: B -> Int -> Int -> Bool
read B { width = w, height = h, body = bd } x y =
	testBit (bd V.! (w' * y' + (x' `div` 8))) $ 7 - x' `mod` 8
	where w' = (w - 1) `div` 8 + 1; x' = x `mod` w; y' = y `mod` h

generate :: Int -> Int -> (Int -> Int -> Bool) -> B
generate w h px = B { width = w, height = h, body = generateBody w h px }

generateBody :: Int -> Int -> (Int -> Int -> Bool) -> V.Vector Word8
generateBody w h px = V.generate (w' * h) \i ->
	boolsToWord $ (<$> [0 .. 7]) \x -> px' (i `mod` w' * 8 + x) (i `div` w')
	where
	px' x y	| x >= w = False | otherwise = px x y
	w' = (w - 1) `div` 8 + 1

boolsToWord :: [Bool] -> Word8
boolsToWord bls = go 0 $ bls ++ replicate (8 - length bls) False
	where go r = \case
		[] -> r; b : bs -> go (bool id (`setBit` 0) b $ r `shiftL` 1) bs

checkTopEdge :: B -> Bool
checkTopEdge B { width = w, height = _h, body = bd } =
	any (/= 0) $ (bd V.!) <$> [0 .. (w - 1) `div` 8]

checkBottomEdge :: B -> Bool
checkBottomEdge B { width = w, height = h, body = bd } =
	any (/= 0) $ ((bd V.!) . (btt +)) <$> [0 .. w' - 1]
	where btt = (h - 1) * w'; w' = (w - 1) `div` 8 + 1

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

enlargeToGray1 :: Int -> B -> Gray1.G
enlargeToGray1 n b = Gray1.generate (width b * n) (height b * n) \x y ->
	read b (x `div` n) (y `div` n)

fromGray1 :: Gray1.G -> B
fromGray1 Gray1.G { Gray1.width = w, Gray1.height = h, Gray1.body = bd } =
	B { width = w, height = h, body = bd }

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

addShapeAscii :: B -> Int -> Int -> [String] -> B
addShapeAscii bd xo yo = addShape bd xo yo . (((== '*') <$>) <$>)

addShape :: B -> Int -> Int -> [[Bool]] -> B
addShape bd xo yo =
	generate (width bd) (height bd) . addShapePixel bd xo yo

addShapePixel :: B -> Int -> Int -> [[Bool]] -> Int -> Int -> Bool
addShapePixel bd xo yo bss x y
	|	xo <= x && x < xo + (length $ head bss) &&
		yo <= y && y < yo + (length bss) = bss !! (y - yo) !! (x - xo)
	| otherwise = read bd x y

-- MATCHING

multiMatchLivesTop :: Ord a => Int ->
	[(a, Pattern)] -> B -> ([(Int, Int)], [(a, (Int, Int))])
multiMatchLivesTop n pttns brd = second (L.nub . L.sort) . partitionEithers
	$ (<$> boardLivesTop n brd) \(x, y) -> (\(i, (px, py)) -> (i, (x - px, y - py))) <$> multiMatchLife pttns x y brd

multiMatchLivesBottom :: Ord a => Int ->
	[(a, Pattern)] -> B -> ([(Int, Int)], [(a, (Int, Int))])
multiMatchLivesBottom n pttns brd = second (L.nub . L.sort) . partitionEithers
	$ (<$> boardLivesBottom n brd) \(x, y) -> (\(i, (px, py)) -> (i, (x - px, y - py))) <$> multiMatchLife pttns x y brd

boardLivesBottom :: Int -> B -> [(Int, Int)]
boardLivesBottom n bd@B { width = w, height = h } =
	[ (x, y) | x <- [0 .. w - 1], y <- [h - n .. h - 1], read bd x y ]

boardLivesTop :: Int -> B -> [(Int, Int)]
boardLivesTop n bd@B { width = w, height = _h } =
	[ (x, y) | x <- [0 .. w - 1], y <- [0 .. n - 1], read bd x y ]

multiMatchLife :: [(a, Pattern)] -> Int -> Int -> B -> Either (Int, Int) (a, (Int, Int))
multiMatchLife pttns bx by brd = maybe (Left (bx, by)) Right . listToMaybe . catMaybes $
	(\(x, pttn) -> (x ,) <$> matchLife pttn bx by brd) <$> pttns

matchLife :: Pattern -> Int -> Int -> B -> Maybe (Int, Int)
matchLife pttn@Pattern { patternLives = lvs0 } bx by brd = go lvs0
	where
	go [] = Nothing
	go ((px, py) : lvs) =
		bool (go lvs) (Just (px, py)) $ match px py pttn bx by brd

match :: Int -> Int -> Pattern -> Int -> Int -> B -> Bool
match px py pttn@Pattern { patternWidth = pw, patternHeight = ph } bx by brd =
	matchClipped pttn $ clip brd (bx - px) (by - py) pw ph

matchClipped :: Pattern -> Clipped -> Bool
matchClipped Pattern { patternBody = pbd } Clipped { clippedBody = cbd } =
	pbd == cbd

clip :: B -> Int -> Int -> Int -> Int -> Clipped
clip B { width = bw, height = bh, body = bbd } cxo cyo cw ch =
	Clipped {
		clippedWidth = cw, clippedHeight = ch,
		clippedBody = V.generate
			(((cw - 1) `div` 8 + 1) * ch) (clipBodyFun' bw bh bbd cxo cyo cw ch) }

data Clipped = Clipped {
	clippedWidth :: Int, clippedHeight :: Int, clippedBody :: V.Vector Word8 }
	deriving (Generic, Eq, Show)

clipBodyFun' ::
	Int -> Int -> V.Vector Word8 -> Int -> Int -> Int -> Int -> Int -> Word8
clipBodyFun' w h bd cxo cyo cw _ch i
	| (cxow + xw) < (- 1) || (cxow + xw) >= ww = 0
	| (cyo + y) < 0 || (cyo + y) >= h = 0
	| (cxow + xw) < 0 = combine cxob 0 (bd V.! (((cyo + y) * ww) + cxow + xw + 1))
	| (cxow + xw) < (ww - 1) = combine cxob
		(bd V.! (((cyo + y) * ww) + cxow + xw))
		(bd V.! (((cyo + y) * ww) + cxow + xw + 1))
	| otherwise = combine cxob (bd V.! (((cyo + y) * ww) + cxow + xw)) 0
	where
	xw = i `mod` cww
	y = i `div` cww
	ww = (w - 1) `div` 8 + 1
	cww = (cw - 1) `div` 8 + 1
	cxow = cxo `div` 8
	cxob = cxo `mod` 8

combine :: Int -> Word8 -> Word8 -> Word8
combine n b1 b2 = b1 `shiftL` n .|. b2 `shiftR` (8 - n)

data Pattern = Pattern {
	patternLives :: [(Int, Int)],
	patternWidth :: Int,
	patternHeight :: Int,
	patternBody :: V.Vector Word8 }
	deriving Show

asciiToPattern :: Int -> Int -> Int -> Int -> [String] -> Pattern
asciiToPattern w h xo yo = boolsToPattern w h xo yo . (((== '*') <$>) <$>)

boolsToPattern :: Int -> Int -> Int -> Int -> [[Bool]] -> Pattern
boolsToPattern w h xo yo bs = Pattern {
	patternLives = boolsToLives xo yo bs,
	patternWidth = w, patternHeight = h,
	patternBody = putShapeBody w h xo yo bs }

putShapeBody :: Int -> Int -> Int -> Int -> [[Bool]] -> V.Vector Word8
putShapeBody w h xo yo = generateBody w h . putShapePixel xo yo

boolsToLives :: Int -> Int -> [[Bool]] -> [(Int, Int)]
boolsToLives xo yo bs = go xo yo bs
	where
	go _ _ [] = []
	go _x y ([] : rs) = go xo (y + 1) rs
	go x y ((lf : lvs) : rs) = bool id ((x, y) :) lf $ go (x + 1) y (lvs : rs)

-- CLEAR

data Area =
	Area { arLeft :: Int, arTop :: Int, arWidth :: Int, arHeight :: Int }
	deriving Show

clear :: B -> [Area] -> B
clear brd@B { width = w, height = h } ars =
	generate w h \x y -> read brd x y && not (insideAreas ars x y)

insideAreas :: [Area] -> Int -> Int -> Bool
insideAreas ars x y = any inside ars
	where inside (Area l u w h) = l <= x && x < l + w && u <= y && y < u + h
