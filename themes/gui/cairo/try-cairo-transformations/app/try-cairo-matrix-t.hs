{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Graphics.Cairo.Utilities.CairoMatrixT

main :: IO ()
main = do
	print sampleMatrix1

sampleMatrix1 :: Matrix
sampleMatrix1 = runST do
	m <- cairoMatrixNew
	cairoMatrixInit m 5 8 10 18 3 2
	cairoMatrixGet m
