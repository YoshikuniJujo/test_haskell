{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox.View (
	View, View1(..), Color(..), Position, FontSize, LineWeight ) where

import Data.Word (Word8)
import Data.Text (Text)
import Codec.Picture (Image, PixelRGBA8)

type View n = [View1 n]

data View1 n
	= Text Color FontSize (Position n) Text
	| Image (Position n) (Image PixelRGBA8)
	| Line Color (LineWeight n) (Position n) (Position n)

data Color =
	Color { colorRed :: Word8, colorGreen :: Word8, colorBlue :: Word8 }
	deriving Show

type Position n = (n, n)
type FontSize = Double
type LineWeight n = n
