{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.View (
	-- * View
	View, View1(..), view,
	-- * Color
	Color, white, blue ) where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import Data.Vector.Storable (Storable, MVector, modify)
import Data.Bits ((.|.), shiftL)
import Codec.Picture (Image(imageWidth, imageHeight, imageData), PixelRGBA8)

import qualified Data.Text as T
import qualified Data.Vector.Generic.Mutable as MV

import Trial.Followbox.ViewType (View, View1(..), Color(..), white, blue)
import Field (
	Field, Pixel, Position,
	clearField, flushField, drawLine, drawStr, drawImage )

---------------------------------------------------------------------------

-- * VIEW
-- * DRAW IMAGE PIXEL

---------------------------------------------------------------------------
-- VIEW
---------------------------------------------------------------------------

view :: Field -> View -> IO ()
view f v = clearField f >> view1 f `mapM_` v >> flushField f

view1 :: Field -> View1 -> IO ()
view1 f (Text
	(colorToPixel -> p) fn fs
	(x, y)
	(T.unpack -> s)) = drawStr f p fn fs (round x) (round y) s
view1 f (Line
	(colorToPixel -> p) (fromIntegral -> lw)
	(round -> x1, round -> y1)
	(round -> x2, round -> y2)) = drawLine f p lw x1 y1 x2 y2
view1 f (Image
	(round -> x, round -> y) img) = drawImagePixel f img x y

colorToPixel :: Color -> Pixel
colorToPixel Color {
	colorRed = fromIntegral -> r,
	colorGreen = fromIntegral -> g,
	colorBlue = fromIntegral -> b } = r `shiftL` 16 .|. g `shiftL` 8 .|. b

---------------------------------------------------------------------------
-- DRAW IMAGE PIXEL
---------------------------------------------------------------------------

drawImagePixel :: Field -> Image PixelRGBA8 -> Position -> Position -> IO ()
drawImagePixel f img x y = drawImage f dt x y w h
	where
	dt = modify swap02s $ imageData img
	w = fromIntegral $ imageWidth img
	h = fromIntegral $ imageHeight img

swap02s :: Storable a => MVector s a -> ST s ()
swap02s v = zipWithM_ (MV.swap v) [0, 4 .. MV.length v] [2, 6 .. MV.length v]
