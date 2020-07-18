{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import MergeSortIm (mergesortim)
import Data.List
import RewriteSort (sort)

import ForProfile

main :: IO ()
main = do
	print . last $ {-# SCC "MergeSortIm.mergesortim" #-} mergesortim sample1_100000
	print . last $ {-# SCC "RewriteSort.sort" #-} RewriteSort.sort sample1_100000
	print . last $ {-# SCC "Data.List.sort" #-} Data.List.sort sample1_100000
	print . last $ {-# SCC "MergeSortIm.mergesortim_X" #-} mergesortim $ sampleX_N 100
	print . last $ {-# SCC "RewriteSort.sort_X" #-} RewriteSort.sort $ sampleX_N 100
	print . last $ {-# SCC "Data.List.sort_X" #-} Data.List.sort $ sampleX_N 100
