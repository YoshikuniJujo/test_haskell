{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox.Viewer (View, view) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Text
import Codec.Picture (Image(imageWidth, imageHeight, imageData), PixelRGBA8)

import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Storable as V

import Followbox.View
import Field as F

view :: Integral n => Field -> View n -> IO ()
view f v = do
	clearField f
	view1 f `mapM_` v
	flushField f

view1 :: Integral n => Field -> View1 n -> IO ()
view1 f (Text c fs (x, y) t) =
	drawStr f (colorToPixel c) "sans" fs (fromIntegral x) (fromIntegral y) $ unpack t
view1 f (Image (x, y) img) = drawImagePixel f img (fromIntegral x) (fromIntegral y)
view1 f (Line c lw (xs, ys) (xe, ye)) =
	drawLine f (colorToPixel c) (fromIntegral lw)
		(fromIntegral xs) (fromIntegral ys)
		(fromIntegral xe) (fromIntegral ye)

colorToPixel :: Color -> F.Pixel
colorToPixel Color { colorRed = r_, colorGreen = g_, colorBlue = b_ } =
	r `shiftL` 16 .|. g `shiftL` 8 .|. b
	where [r, g, b] = fromIntegral <$> [r_, g_, b_]

drawImagePixel :: Field -> Image PixelRGBA8 -> F.Position -> F.Position -> IO ()
drawImagePixel f img x y = do
	let	w = fromIntegral $ imageWidth img
		h = fromIntegral $ imageHeight img
		dt = V.modify swap02s $ imageData img
	F.drawImage f dt x y w h

swap02s :: V.Storable a => V.MVector s a -> ST s ()
swap02s v = zipWithM_ (MV.swap v) [0, 4 .. MV.length v] [2, 6 .. MV.length v]
