{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.JuicyCairo
import Codec.Picture

import SampleImages

main :: IO ()
main = writePng "helloWriteImageSurface.png"
	$ cairoArgb32ToJuicyRGBA8 twoRectangles
