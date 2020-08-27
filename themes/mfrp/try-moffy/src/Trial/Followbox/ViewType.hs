{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.ViewType (
	-- * VIEW
	View, View1(..),
	-- * COLOR
	Color(..), white, blue ) where


import Data.Word (Word8)
import Codec.Picture (Image, PixelRGBA8)

import qualified Data.Text as T

import Trial.Followbox.TypeSynonym (Position, LineWidth, FontName, FontSize)

---------------------------------------------------------------------------

type View = [View1]

data View1
	= Text Color FontName FontSize Position T.Text
	| Line Color LineWidth Position Position
	| Image Position (Image PixelRGBA8)

data Color =
	Color { colorRed :: Word8, colorGreen :: Word8, colorBlue :: Word8 }
	deriving Show

white, blue :: Color
white = Color { colorRed = 0xff, colorGreen = 0xff, colorBlue = 0xff }
blue = Color { colorRed = 0x30, colorGreen = 0x66, colorBlue = 0xd6 }
