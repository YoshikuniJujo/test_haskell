{-# LANGUAGE QuasiQuotes, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Minos (Mino, mkMinos, standardMinos, rotateMinoL, rotateMinoR, minoToPos) where

import Control.Arrow
import Data.List
import Text.Nowdoc

import Rotate

newtype Mino = Mino { getMino :: [Pos] } deriving Show

minoToPos :: Mino -> Pos -> [Pos]
minoToPos (Mino ps) (x, y) = ((+ x) *** (+ y)) <$> ps

mkMinos :: String -> [Mino]
mkMinos = (centerize . linesToMino <$>) . separateMino

separateMino :: String -> [[String]]
separateMino = separate (maybe False ((== '-') . fst) . uncons)
	. filter (not . null) . (dropWhile (== '\t') <$>) . lines

separate :: (a -> Bool) -> [a] -> [[a]]
separate _ [] = [[]]
separate p (x : xs)
	| p x = [] : separate p xs
	| otherwise = case separate p xs of
		[] -> error "never occur"
		ys : yss -> (x : ys) : yss

linesToMino :: [String] -> Mino
linesToMino = Mino . concat . zipWith (\y xs -> map (, y) xs) [0 ..] . ((fst <$>) . filter ((== '*') . snd) . zip [0 ..] <$>)

{-
minoI, minoJ, minoL, minoO, minoS, minoT, minoZ :: Mino
[minoI, minoJ, minoL, minoO, minoS, minoT, minoZ] = standardMinos
-}

standardMinos :: [Mino]
standardMinos = centerize . linesToMino <$> separateMino [nowdoc|
	****
	----
	*
	***
	----
	  *
	***
	----
	**
	**
	----
	 **
	**
	----
	 *
	***
	----
	**
	 ** |]

moveMino :: Int -> Int -> Mino -> Mino
moveMino x y = Mino . (((+ x) *** (+ y)) <$>) . getMino

centerize :: Mino -> Mino
centerize m = let (cx, cy) = center m in moveMino (- round cx) (- round cy) m

center :: Mino -> PosR
center = (average *** average) . unzip . getMino

average :: Integral n => [n] -> Rational
average ns = fromIntegral (sum ns) / fromIntegral (length ns)

dist :: PosR -> Pos -> Rational
dist (cx, cy) (x, y) = abs (fromIntegral x - cx) + abs (fromIntegral y - cy)

sortCenter :: Mino -> [Center]
sortCenter m@(Mino ps) = nub $ nearestCenter c : (uncurry Grid <$> sortOn (dist c) ps)
	where c = center m

rotateMinoL, rotateMinoR :: Mino -> [Mino]
rotateMinoL = rotateLeftRight' rotatePosL
rotateMinoR = rotateLeftRight' rotatePosR

rotateLeftRight' :: (Center -> Pos -> Pos) -> Mino -> [Mino]
rotateLeftRight' lr m = rotateLR lr m <$> sortCenter m

rotateLR :: (Center -> Pos -> Pos) -> Mino -> Center -> Mino
rotateLR lr (Mino ps) c = Mino $ lr c <$> ps
