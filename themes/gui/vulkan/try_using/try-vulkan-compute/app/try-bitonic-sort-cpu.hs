{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
import Data.Array
import Data.Word
import Data.Time
import System.Random

import TryBitonicSortCpu

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

main :: IO ()
main = do
	rs <- getRandomRs @Word32 (1, 10 ^ (7 :: Int)) $ 2 ^ (24 :: Int)
	ct0 <- getCurrentTime
	ns <- bitonicSortCpu 24 $ listArray (0, 2 ^ (24 :: Int) - 1) rs
	ct1 <- getCurrentTime
	print . take 10 $ toList ns
	print . checkSorted 0 $ toList ns
	print $ diffUTCTime ct1 ct0

checkSorted :: Ord a => Int -> [a] -> (Int, Bool)
checkSorted i [_] = (i, True)
checkSorted i (x : xs@(y : _))
	| x <= y = checkSorted (i + 1) xs
	| otherwise = (i, False)
