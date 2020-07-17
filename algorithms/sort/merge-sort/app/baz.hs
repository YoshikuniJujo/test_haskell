{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import MergeSortIm (mergesortim)
import Data.List
import System.Random

sample0 :: [Int]
sample0 = take 1000 . randomRs (1, 1000) $ mkStdGen 8

sampleN :: Int -> [Int]
sampleN n = [1 .. 10000000] ++ take (n * 1000) (cycle [10000001, 10000002])

sample1 :: [Int]
sample1 = [1 .. 1000000] ++ take 100000 (cycle [1000001, 1000002])

sample5 :: [Int]
sample5 = [1 .. 1000000] ++ take 500000 (cycle [1000001, 1000002])

main :: IO ()
main = do
--	print . last $ {-# SCC "Data.List.sort" #-} sort sample1
--	print . last $ {-# SCC "MergeSortIm.mergesortim" #-} mergesortim sample1
--	print . last $ {-# SCC "Data.List.sort_1000" #-} sort (sampleN 1000)
	print . last $ {-# SCC "MergeSortIm.mergesortim_1000" #-} mergesortim (sampleN 1000)
--	print . last $ {-# SCC "Data.List.sort_5000" #-} sort (sampleN 5000)
--	print . last $ {-# SCC "MergeSortIm.mergesortim_5000" #-} mergesortim (sampleN 5000)
