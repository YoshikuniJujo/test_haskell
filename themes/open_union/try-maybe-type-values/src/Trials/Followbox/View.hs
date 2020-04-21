{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.View (
	View, View1(..), view, Color, white, blue, Position, FontSize, LineWidth
	) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Word
import Data.Text
import Codec.Picture (Image(imageWidth, imageHeight, imageData), PixelRGBA8)

import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as V

import qualified Field as F

type View = [View1]

data View1
	= Text Color FontSize Position Text
	| Line Color LineWidth Position Position
	| Image Position (Image PixelRGBA8)

data Color =
	Color { colorRed :: Word8, colorGreen :: Word8, colorBlue :: Word8 }
	deriving Show

type Position = (Integer, Integer)
type FontSize = Double
type LineWidth = Integer

view :: F.Field -> View -> IO ()
view f v = do
	F.clearField f
	view1 f `mapM_` v
	F.flushField f

view1 :: F.Field -> View1 -> IO ()
view1 f (Text c fs (x, y) t) = F.drawStr f
	(colorToPixel c) "sans" fs (fromIntegral x) (fromIntegral y) $ unpack t
view1 f (Line c lw_ (xs_, ys_) (xe_, ye_)) =
	F.drawLine f (colorToPixel c) (fromIntegral lw_) xs ys xe ye
	where [xs, ys, xe, ye] = fromIntegral <$> [xs_, ys_, xe_, ye_]
view1 f (Image (x, y) img) = drawImagePixel f img (fromIntegral x) (fromIntegral y)

colorToPixel :: Color -> F.Pixel
colorToPixel Color { colorRed = r_, colorGreen = g_, colorBlue = b_ } =
	r `shiftL` 16 .|. g `shiftL` 8 .|. b
	where [r, g, b] = fromIntegral <$> [r_, g_, b_]

white, blue :: Color
white = Color { colorRed = 0xff, colorGreen = 0xff, colorBlue = 0xff }
blue = Color { colorRed = 0x30, colorGreen = 0x66, colorBlue = 0xd6 }

drawImagePixel :: F.Field -> Image PixelRGBA8 -> F.Position -> F.Position -> IO ()
drawImagePixel f img x y = do
	let	w = fromIntegral $ imageWidth img
		h = fromIntegral $ imageHeight img
		dt = V.modify swap02s $ imageData img
	F.drawImage f dt x y w h

swap02s :: V.Storable a => V.MVector s a -> ST s ()
swap02s v = zipWithM_ (MV.swap v) [0, 4 .. MV.length v] [2, 6 .. MV.length v]
