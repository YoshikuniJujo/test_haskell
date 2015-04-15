{-# LANGUAGE TupleSections #-}

import Data.List

main = interact $ show . length . numSquare . read

numSquare :: Int -> [Int]
numSquare 0 = []
numSquare n = s : numSquare (n - s)
	where
	s = last . takeWhile (<= n) $ map (^ 2) [1 .. 300]

squares :: [Int]
squares = map (^ 2) [0 .. 300]

patterns :: Int -> [Int] -> [[Int]]
patterns n _ | n < 0 = []
patterns 0 _ = [[]]
patterns _ [] = []
patterns n ca@(c : cs) = map (c :) (patterns (n - c) ca) ++ patterns n cs

mins :: [Int]
mins = [ mn n | n <- [0..90000] ]

mn :: Int -> Int
mn n	| n `elem` squares = 1
	| otherwise = 1 + minimum (map (mins !!) . filter (>= 0) . map (n -)
		$ filter (< n) squares)

marge, marge' :: [a] -> [b] -> [a] -> [b] -> [(a, b)]
marge _ _ [] [] = []
marge [] [] (x : xs) (y : ys) = (x, y) : marge [x] [y] xs ys
marge pxs pys (x : xs) ys = map (x ,) pys ++ marge' (x : pxs) pys xs ys
marge' _ _ [] [] = []
marge' [] [] (x : xs) (y : ys) = (x, y) : marge' [x] [y] xs ys
marge' pxs pys xs (y : ys) = map (, y) pxs ++ marge pxs (y : pys) xs ys

squares2 :: [Int]
squares2 = map (uncurry (+)) $ marge [] [] squares squares
