{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.View (
	-- * VIEW
	View, View1(..), view,
	-- * COLOR
	Color, white, blue ) where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import Data.Bits ((.|.), shiftL)
import Data.Word (Word8)
import Codec.Picture (Image(imageWidth, imageHeight, imageData), PixelRGBA8)

import qualified Data.Text as T
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as V

import Trial.Followbox.TypeSynonym (Position, LineWidth, FontName, FontSize)

import qualified Field as F

---------------------------------------------------------------------------

type View = [View1]

data View1
	= Text Color FontName FontSize Position T.Text
	| Line Color LineWidth Position Position
	| Image Position (Image PixelRGBA8)

data Color =
	Color { colorRed :: Word8, colorGreen :: Word8, colorBlue :: Word8 }
	deriving Show

view :: F.Field -> View -> IO ()
view f v = F.clearField f >> view1 f `mapM_` v >> F.flushField f

view1 :: F.Field -> View1 -> IO ()
view1 f (Text c fn fs (x_, y_) t) = F.drawStr f p fn fs x y $ T.unpack t
	where
	p = colorToPixel c
	[x, y] = fromIntegral <$> [x_, y_]
view1 f (Line c lw_ (x1_, y1_) (x2_, y2_)) = F.drawLine f p lw x1 y1 x2 y2
	where
	p = colorToPixel c; lw = fromIntegral lw_
	[x1, y1, x2, y2] = fromIntegral <$> [x1_, y1_, x2_, y2_]
view1 f (Image (x_, y_) img) = drawImagePixel f img x y
	where [x, y] = fromIntegral <$> [x_, y_]

colorToPixel :: Color -> F.Pixel
colorToPixel Color { colorRed = r_, colorGreen = g_, colorBlue = b_ } =
	r `shiftL` 16 .|. g `shiftL` 8 .|. b
	where [r, g, b] = fromIntegral <$> [r_, g_, b_]

white, blue :: Color
white = Color { colorRed = 0xff, colorGreen = 0xff, colorBlue = 0xff }
blue = Color { colorRed = 0x30, colorGreen = 0x66, colorBlue = 0xd6 }

drawImagePixel ::
	F.Field -> Image PixelRGBA8 -> F.Position -> F.Position -> IO ()
drawImagePixel f img x y = F.drawImage f dt x y w h
	where
	dt = V.modify swap02s $ imageData img
	w = fromIntegral $ imageWidth img
	h = fromIntegral $ imageHeight img

swap02s :: V.Storable a => V.MVector s a -> ST s ()
swap02s v = zipWithM_ (MV.swap v) [0, 4 .. MV.length v] [2, 6 .. MV.length v]
