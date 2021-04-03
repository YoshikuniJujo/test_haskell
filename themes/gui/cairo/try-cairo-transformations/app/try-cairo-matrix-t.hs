{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Graphics.Cairo.Utilities.CairoMatrixT

main :: IO ()
main = print `mapM_` [
	sampleMatrix1, sampleMatrix2, sampleMatrix3, sampleMatrix4,
	sampleMatrix5, sampleMatrix6 ]

sampleMatrix1, sampleMatrix2, sampleMatrix3, sampleMatrix4, sampleMatrix5,
	sampleMatrix6 :: Matrix
sampleMatrix1 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInit m 5 8 10 18 3 2 >> cairoMatrixGet m

sampleMatrix2 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInitIdentity m >> cairoMatrixGet m

sampleMatrix3 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInitTranslate m 15 25 >> cairoMatrixGet m

sampleMatrix4 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInitScale m 3 8 >> cairoMatrixGet m

sampleMatrix5 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInitRotate m (pi / 2) >> cairoMatrixGet m

sampleMatrix6 = runST $ cairoMatrixNew >>= \m ->
	cairoMatrixInitRotate m (pi / 4) >> cairoMatrixGet m
