{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FollowboxView (View, View1(..), Color(..)) where

import Data.Word (Word8)
import Data.Text (Text)
import Codec.Picture (Image, PixelRGBA8)

type View n = [View1 n]

data View1 n
	= Text Color FontSize (Position n) Text
	| Image (Position n) (Image PixelRGBA8)
	| Line Color (LineWeight n) (Position n) (Position n)

type FontSize = Double
type LineWeight n = n
type Position n = (n, n)

data Color =
	Color { colorRed :: Word8, colorGreen :: Word8, colorBlue :: Word8 }
	deriving Show
