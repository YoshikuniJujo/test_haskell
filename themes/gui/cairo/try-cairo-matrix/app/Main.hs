{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST

import Lib

main :: IO ()
main = print sampleMatrix1

sampleMatrix1 :: Matrix
sampleMatrix1 = runST $ cairoMatrixNew 3 5 8 13 2 9 >>= \case
	Left m -> cairoMatrixGet m
	Right m -> cairoMatrixGet m
