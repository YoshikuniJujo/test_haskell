{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Graphics.Cairo.Utilities.CairoMatrixT

main :: IO ()
main = print `mapM_` [sampleMatrix1, sampleMatrix2, sampleMatrix3]

sampleMatrix1, sampleMatrix2, sampleMatrix3 :: Matrix
sampleMatrix1 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInit m 5 8 10 18 3 2 >> cairoMatrixGet m

sampleMatrix2 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInitIdentity m >> cairoMatrixGet m

sampleMatrix3 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInitTranslate m 15 25 >> cairoMatrixGet m
