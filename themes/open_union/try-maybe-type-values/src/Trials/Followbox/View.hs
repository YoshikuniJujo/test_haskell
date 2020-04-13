{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.View (
	view, View, View1(..), Color, FontSize, Position,
	white, blue
	) where

import Data.Bits
import Data.Word
import Data.Text

import qualified Field as F

type View = [View1]

data View1
	= Text Color FontSize Position Text
	deriving Show

data Color =
	Color { colorRed :: Word8, colorGreen :: Word8, colorBlue :: Word8 }
	deriving Show

type Position = (Integer, Integer)
type FontSize = Double

view :: F.Field -> View -> IO ()
view f v = do
	F.clearField f
	view1 f `mapM_` v
	F.flushField f

view1 :: F.Field -> View1 -> IO ()
view1 f (Text c fs (x, y) t) =
	F.drawStr f (colorToPixel c) "sans" fs (fromIntegral x) (fromIntegral y) $ unpack t

colorToPixel :: Color -> F.Pixel
colorToPixel Color { colorRed = r_, colorGreen = g_, colorBlue = b_ } =
	r `shiftL` 16 .|. g `shiftL` 8 .|. b
	where [r, g, b] = fromIntegral <$> [r_, g_, b_]

white, blue :: Color
white = Color { colorRed = 0xff, colorGreen = 0xff, colorBlue = 0xff }
blue = Color { colorRed = 0x30, colorGreen = 0x66, colorBlue = 0xd6 }
