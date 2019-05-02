{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.List
import Data.Char

powerSet :: [a] -> [[a]]
powerSet = subsequences

unions, commons :: Eq a => [a] -> [[a]] -> [a]
unions us fos = [ x | x <- us, any (x `elem`) fos ]
commons us fos = [ x | x <- us, all (x `elem`) fos]

data A = A Int deriving Show
data B = B Char deriving Show

corres :: A -> [B]
corres (A n)
	| even n = [B $ chr n, B . chr $ n + 1]
	| otherwise = [B . chr $ n - 1, B $ chr n]

graph :: [A] -> (A -> [B]) -> [(A, B)]
graph aa crr = [ (x, y) | x <- aa, y <- crr x ]
