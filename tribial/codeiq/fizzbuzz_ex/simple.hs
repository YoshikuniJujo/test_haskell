import Data.List
import Data.Function

ex1 = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1 .. 20]
ex2 = sum $ filter (\n -> any ((== 0) . (n `mod`)) [3, 5]) [1 .. 1000]
ex3 = sum $ filter (\n -> any ((== 0) . (n `mod`))
	[3, 5, 7, 11, 13, 17, 19, 23, 29, 31]) [1 .. 1000]

sumOf :: Integer -> Integer -> Integer
sumOf n p = p * (n `div` p) * (n `div` p + 1) `div` 2

ex1' = sumOf 20 3 + sumOf 20 5 - sumOf 20 15
ex2' = sumOf 1000 3 + sumOf 1000 5 - sumOf 1000 15

partials :: [a] -> [[a]]
partials [] = [[]]
partials (x : xs) = partials xs ++ map (x :) (partials xs)

nums :: Num a => [a] -> [[a]]
nums = tail .
	map (map product) .
	groupBy ((==) `on` length) . sortBy (compare `on` length) . partials

sumOfs :: Integer -> [Integer] -> Integer
sumOfs n = sum . map (sumOf n)

sumOfss :: Integer -> [[Integer]] -> Integer
sumOfss n [] = 0
sumOfss n (p : ps) = sumOfs n p - sumOfss n ps
