{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck.All
import Test.QuickCheck

import Data.Bits
import Debug.Trace
import Data.List

prop_eqSelfInt :: Int -> Bool
prop_eqSelfInt x = x == x

prop_smallerThan100 :: Int -> Bool
prop_smallerThan100 = (< 100)

prop_exp2 :: Integer -> Integer -> Bool
prop_exp2 x y = x `shiftL` fromInteger y ==
	floor (fromInteger x * 2 ** fromInteger y)

trace' :: Show a => a -> a
trace' x = traceShow x x

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted xs@(_ : txs) = and $ zipWith (<=) xs txs

prop_sorted :: [Int] -> Bool
prop_sorted = isSorted . sort

main = $quickCheckAll

main' = $forAllProperties $ quickCheckWithResult $ stdArgs{ maxSuccess = 50 }
