{-# LANGUAGE BangPatterns, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.DeepSeq
import Data.Time
import TryBitonicsortGpu
import ForCheck

listSize :: Integral n => n
listSize = 25

main :: IO ()
main = getRandomRs (1, 10 ^ (8 :: Int))
		(2 ^ (listSize :: Int)) >>= \(force -> !rs) -> do
	ct0 <- getCurrentTime
	rslt <- bitonicsortGpu listSize rs
	ct1 <- getCurrentTime

	print $ take 20 rslt
	print $ checkSorted 0 rslt
	print $ diffUTCTime ct1 ct0

	print $ length rs
