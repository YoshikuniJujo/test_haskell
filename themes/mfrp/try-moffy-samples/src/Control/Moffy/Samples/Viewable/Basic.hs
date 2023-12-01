{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Viewable.Basic (
	Color(..), white, blue, colorToRgb, Position, LineWidth ) where

import Data.Word (Word8)

data Color =
	Color { colorRed :: Word8, colorGreen :: Word8, colorBlue :: Word8 }
	deriving Show

white, blue :: Color
white = Color { colorRed = 0xff, colorGreen = 0xff, colorBlue = 0xff }
blue = Color { colorRed = 0x30, colorGreen = 0x66, colorBlue = 0xd6 }

colorToRgb :: Color -> (Double, Double, Double)
colorToRgb (Color (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b)) =
	(r / 0xff, g / 0xff, b / 0xff)

type Position = (Double, Double)
type LineWidth = Double
