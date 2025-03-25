{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.ST
import System.Environment

import Data.Foldable
import Data.Vector.Mutable qualified as V
import Data.Word

import Try.CheckImage

main :: IO ()
main = do
	fp : _ <- getArgs
	v <- V.new $ 16 * 16 * 4
	for_ [0 .. 15] \y -> for_ [0 .. 15] \x ->
		putPixelToVector v 16 16 x y
			(fromIntegral x * 8 + fromIntegral y * 8)
			(255 - fromIntegral x * 8 - fromIntegral y * 8)
			0 255
	checkImage 16 16 fp v

putPixelToVector :: V.MVector RealWorld Word8 ->
	Int -> Int -> Int -> Int -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
putPixelToVector v w _ x y r g b a = for_ ([0 .. 3] `zip` [r, g, b, a]) \(i, c) ->
	V.write v ((y * w + x) * 4 + i) c
