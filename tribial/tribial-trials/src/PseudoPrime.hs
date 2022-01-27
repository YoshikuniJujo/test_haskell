{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PseudoPrime where

import Data.List

pseudoPrime :: [Int]
pseudoPrime = filter (not . all3) . filter ((/= 0) . (`mod` 11))
	. filter ((/= 0) . (`mod` 5)) . filter odd
	. filter notPrime $ [10 .. 99] \\ nineNine

nineNine :: [Int]
nineNine = [ a * b | a <- [1 .. 9], b <- [a .. 9] ]

notPrime :: Int -> Bool
notPrime n = not . null $ filter ((== 0) . (n `mod`)) [2 .. n - 1]

all3 :: Int -> Bool
all3 = all ((== 0) . (`mod` 3)) . ten

ten :: Int -> [Int]
ten n | n < 1 = []
ten n = (n `mod` 10) : ten (n `div` 10)
