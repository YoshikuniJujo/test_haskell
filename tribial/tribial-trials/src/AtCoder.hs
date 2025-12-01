{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AtCoder where

import Data.List qualified as L

foo :: Integer -> [[Integer]]
foo n = L.sort $ L.permutations [1 .. n]

bar :: Integer -> [Integer] -> [Integer] -> Integer
bar _ [] [] = 0
bar n (x : xs) ya@(y : _) =
--	product [1 .. n] * (x - y) + bar (n - 1) xs (filter (/= x) ya)
	product [1 .. n] * (x - y) + bar (n - 1) (baz x xs) (baz x ya)

baz :: Integer -> [Integer] -> [Integer]
baz _ [] = []
baz n (x : xs)
	| x < n = x : baz n xs
	| x > n = x - 1 : baz n xs
	| otherwise = baz n xs

indexOf :: [Integer] -> Integer
indexOf ns = bar (fromIntegral $ length ns - 1) ns (L.sort ns)

indexOf' :: [Integer] -> Maybe Int
indexOf' ns = L.elemIndex ns $ foo (fromIntegral $ length ns)

hoge :: [Integer]
hoge = scanl (*) 1 [1 ..]

piyo :: Integer -> [Integer]
piyo n = reverse $ takeWhile (<= n) hoge

boo :: Integer -> [Integer] -> [Integer]
boo _ [] = []
boo x (n : ns) = x `div` n : boo (x `mod` n) ns

bee :: Integer -> [Integer]
bee x = boo x (piyo x)

hige :: [Integer] -> [Integer] -> [Integer]
hige [] _ = []
hige (x : xs) na@(n : _) = y : hige xs (filter (/= y) na)
	where y = na !! fromIntegral x

bazbaz :: Integer -> [Integer] -> [Integer]
bazbaz _ [] = []
bazbaz x (n : ns)
	| n == x = (+ 1) <$> ns
	| otherwise = n : bazbaz x ns

hihige :: Integer -> [Integer]
hihige i = hige ns [1 .. ]
	where ns = bee i

-- fromIndex :: Int -> [Int]
-- fromIndex
