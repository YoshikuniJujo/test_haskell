{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST

import Lib

main :: IO ()
main = do
	print `mapM_` [
		sampleMatrix1, sampleMatrix2, sampleMatrix3, sampleMatrix4,
		sampleMatrix5, sampleMatrix6, sampleMatrix7, sampleMatrix8,
		sampleMatrix9 ]
	print =<< cairoMatrixRegularNew 3 5 8 13 2 9
	print =<< cairoMatrixRegularNew 3 6 4 8 2 9

sampleMatrix1, sampleMatrix2, sampleMatrix3, sampleMatrix4,
	sampleMatrix5, sampleMatrix6, sampleMatrix7, sampleMatrix8,
	sampleMatrix9 :: Matrix
sampleMatrix1 = runST $ cairoMatrixGet =<< cairoMatrixNew 3 5 8 13 2 9
sampleMatrix2 = runST $ cairoMatrixGet =<< cairoMatrixNewIdentity
sampleMatrix3 = runST $ cairoMatrixGet =<< cairoMatrixNewTranslate 5 15
sampleMatrix4 = runST $ cairoMatrixGet =<< cairoMatrixNewScale 3 0
sampleMatrix5 = runST $ cairoMatrixGet =<< cairoMatrixNewRotate (pi / 6)
sampleMatrix6 = runST do
	mtx <- cairoMatrixNew 3 5 8 13 2 9
	cairoMatrixTranslate mtx 10 100
	cairoMatrixGet mtx
sampleMatrix7 = runST do
	mtx <- cairoMatrixNew 3 5 8 19 2 9
	cairoMatrixRotate mtx (pi / 4)
	cairoMatrixGet mtx
sampleMatrix8 = runST do
	mtx <- cairoMatrixNewRotate (pi / 6)
	cairoMatrixInvert mtx
	cairoMatrixGet mtx
sampleMatrix9 = runST do
	a <- cairoMatrixNewTranslate 5 15
	b <- cairoMatrixNewRotate (pi / 6)
	cairoMatrixMultiply a a b
	cairoMatrixGet a
