{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Graphics.Cairo.Utilities.CairoMatrixT

main :: IO ()
main = print `mapM_` [
	sampleMatrix1 ]

sampleMatrix1 :: Matrix
sampleMatrix1 = runST $ cairoMatrixGet =<< cairoMatrixNew 5 8 10 18 3 2
