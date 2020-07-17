{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import MergeSortIm (mergesortim)
import Data.List
import System.Random

sample0 :: [Int]
sample0 = take 1000 . randomRs (1, 1000) $ mkStdGen 8

sampleN :: Int -> [Int]
sampleN n = [1 .. 1000000] ++ take (n * 1000) (cycle [1000001, 1000002])

sample1 :: [Int]
sample1 = [1 .. 1000000] ++ take 100000 (cycle [1000001, 1000002])

sample5 :: [Int]
sample5 = [1 .. 1000000] ++ take 500000 (cycle [1000001, 1000002])

main :: IO ()
main = do
--	print . last $ {-# SCC "Data.List.sort" #-} sort sample1
--	print . last $ {-# SCC "MergeSortIm.mergesortim" #-} mergesortim sample1
	print . last $ {-# SCC "Data.List.sort_1" #-} sort (sampleN 1)
	print . last $ {-# SCC "MergeSortIm.mergesortim_1" #-} mergesortim (sampleN 1)
	print . last $ {-# SCC "Data.List.sort_5" #-} sort (sampleN 5)
	print . last $ {-# SCC "MergeSortIm.mergesortim_5" #-} mergesortim (sampleN 5)
	print . last $ {-# SCC "Data.List.sort_10" #-} sort (sampleN 10)
	print . last $ {-# SCC "MergeSortIm.mergesortim_10" #-} mergesortim (sampleN 10)
	print . last $ {-# SCC "Data.List.sort_50" #-} sort (sampleN 50)
	print . last $ {-# SCC "MergeSortIm.mergesortim_50" #-} mergesortim (sampleN 50)
	print . last $ {-# SCC "Data.List.sort_100" #-} sort (sampleN 100)
	print . last $ {-# SCC "MergeSortIm.mergesortim_100" #-} mergesortim (sampleN 100)
	print . last $ {-# SCC "Data.List.sort_500" #-} sort (sampleN 500)
	print . last $ {-# SCC "MergeSortIm.mergesortim_500" #-} mergesortim (sampleN 500)
