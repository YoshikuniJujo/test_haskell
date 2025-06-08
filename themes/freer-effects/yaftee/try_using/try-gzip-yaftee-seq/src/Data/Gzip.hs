{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Gzip where

codeLengthList :: [Int]
codeLengthList =
	[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

fixedHuffmanList, fixedHuffmanDstList :: [Int]
fixedHuffmanList =
	replicate 144 8 ++ replicate 112 9 ++ replicate 24 7 ++ replicate 8 8

fixedHuffmanDstList = replicate 32 5
