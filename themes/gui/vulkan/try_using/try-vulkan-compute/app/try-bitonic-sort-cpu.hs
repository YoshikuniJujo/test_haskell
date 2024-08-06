{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Exception
import Data.Foldable
import Data.Word
import Data.Time
import System.Random

import TryBitonicSortCpu

import Control.DeepSeq

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

main :: IO ()
main = do
	rs <- evaluate . force =<< getRandomRs @Word32 (1, 10 ^ (8 :: Int)) (2 ^ (25 :: Int))

	ct0 <- getCurrentTime

	ns <- bitonicSortCpu' 25 rs

	ct1 <- getCurrentTime

	print . take 20 $ toList ns
	print . checkSorted 0 $ toList ns

	print $ diffUTCTime ct1 ct0

checkSorted :: Ord a => Int -> [a] -> (Int, Bool)
checkSorted i [] = (i - 1, True)
checkSorted i [_] = (i, True)
checkSorted i (x : xs@(y : _))
	| x <= y = checkSorted (i + 1) xs
	| otherwise = (i, False)
