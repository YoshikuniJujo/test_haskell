{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Foldable
import Data.Word
import Data.Time
import System.Random

import Control.DeepSeq
import Control.Exception

import TryQuickSort
import TryBitonicSortCpu
import TryBitonicSortGpu

import System.Environment

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

main :: IO ()
main = do
	pln : _ <- getArgs
	!rs <- evaluate . force =<< getRandomRs @Word32 (1, 10 ^ (8 :: Int)) (2 ^ (25 :: Int))
	for_ pln \case
		'q' -> time $ quicksort 100 rs
		'c' -> time $ bitonicSortCpu 25 rs
		'g' -> time $ bitonicSortGpu 25 rs
		_ -> pure ()

time :: IO [Word32] -> IO ()
time a = do
	ct0 <- getCurrentTime
	!ns <- a
	ct1 <- getCurrentTime
	print $ take 20 ns
	print $ checkSorted 0 ns
	print $ diffUTCTime ct1 ct0

checkSorted :: Ord a => Int -> [a] -> (Int, Bool)
checkSorted i [] = (i - 1, True)
checkSorted i [_] = (i, True)
checkSorted i (x : xs@(y : _))
	| x <= y = checkSorted (i + 1) xs
	| otherwise = (i, False)
