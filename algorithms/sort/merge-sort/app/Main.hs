{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import MergeSortIm (mergesortim)
import Data.List
-- import RewriteSort (sort)
import System.Random

import ForProfile

-- sampleN :: Int -> [Int]
-- sampleN n = [1 .. 1000000] ++ take (n * 1000) (cycle [1000001, 1000002])

sample1 :: [Int]
sample1 = [1 .. 1000000] ++ take 100000 (cycle [1000001, 1000002])

sample5 :: [Int]
sample5 = [1 .. 1000000] ++ take 500000 (cycle [1000001, 1000002])

main :: IO ()
main = do
--	print . last $ {-# SCC "Data.List.sort" #-} sort sample1
--	print . last $ {-# SCC "MergeSortIm.mergesortim" #-} mergesortim sample1
	print . last $ {-# SCC "Data.List.sort_1" #-} sort (sampleX_N 1)
	print . last $ {-# SCC "MergeSortIm.mergesortim_1" #-} mergesortim (sampleX_N 1)
	print . last $ {-# SCC "Data.List.sort_5" #-} sort (sampleX_N 5)
	print . last $ {-# SCC "MergeSortIm.mergesortim_5" #-} mergesortim (sampleX_N 5)
	print . last $ {-# SCC "Data.List.sort_10" #-} sort (sampleX_N 10)
	print . last $ {-# SCC "MergeSortIm.mergesortim_10" #-} mergesortim (sampleX_N 10)
	print . last $ {-# SCC "Data.List.sort_50" #-} sort (sampleX_N 50)
	print . last $ {-# SCC "MergeSortIm.mergesortim_50" #-} mergesortim (sampleX_N 50)
	print . last $ {-# SCC "Data.List.sort_100" #-} sort (sampleX_N 100)
	print . last $ {-# SCC "MergeSortIm.mergesortim_100" #-} mergesortim (sampleX_N 100)
	print . last $ {-# SCC "Data.List.sort_500" #-} sort (sampleX_N 500)
	print . last $ {-# SCC "MergeSortIm.mergesortim_500" #-} mergesortim (sampleX_N 500)
