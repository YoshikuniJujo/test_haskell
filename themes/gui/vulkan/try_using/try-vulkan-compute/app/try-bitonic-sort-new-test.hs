{-# LANGUAGE BangPatterns, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.DeepSeq
import Data.Time
import System.Random
import TryBitonicsortGpu

listSize :: Integral n => n
listSize = 25

main :: IO ()
main = getRandomRs (1, 10 ^ (8 :: Int)) (2 ^ (listSize :: Int)) >>= \(force -> !rs) -> do

	ct0 <- getCurrentTime

	r' <- bitonicsortGpu listSize rs

	ct1 <- getCurrentTime

	print $ take 20 r'
	print $ checkSorted 0 r'
	print $ diffUTCTime ct1 ct0

	print $ length rs

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

checkSorted :: Ord a => Int -> [a] -> (Int, Bool, [a])
checkSorted i [] = (i, True, [])
checkSorted i [_] = (i, True, [])
checkSorted i (x : xs@(y : _))
	| x <= y = checkSorted (i + 1) xs
	| otherwise = (i, False, [x, y])
