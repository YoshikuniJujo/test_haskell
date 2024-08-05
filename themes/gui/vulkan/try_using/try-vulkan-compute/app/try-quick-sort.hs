{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import TryQuickSort
import Data.Foldable
import Data.Array
import Data.Word
import Data.Time
import System.Random

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

main :: IO ()
main = do
	rs <- getRandomRs @Word32 (1, 10 ^ (7 :: Int)) $ 2 ^ (24 :: Int)
	let	!rs' = listArray (0, 2 ^ (24 :: Int) - 1) rs

	ct0 <- getCurrentTime

	ns <- quicksort 10 rs'

	ct1 <- getCurrentTime

	print . take 20 $ toList ns
	print . checkSorted 0 $ toList ns
	print $ diffUTCTime ct1 ct0

checkSorted :: Ord a => Int -> [a] -> (Int, Bool)
checkSorted i [_] = (i, True)
checkSorted i (x : xs@(y : _))
	| x <= y = checkSorted (i + 1) xs
	| otherwise = (i, False)
