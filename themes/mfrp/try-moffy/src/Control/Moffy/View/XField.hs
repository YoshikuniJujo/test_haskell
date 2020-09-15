{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.View.XField (viewText, viewLine, viewImage) where

import Control.Monad (zipWithM_)
import Control.Monad.ST (ST)
import Data.Vector.Storable (Storable, MVector, modify)
import Data.Bits ((.|.), shiftL)
import Codec.Picture (imageWidth, imageHeight, imageData, PixelRGBA8)
import Codec.Picture.Extra
import Graphics.X11.Xrender

import qualified Data.Text as T
import qualified Data.Vector.Generic.Mutable as MV
import qualified Codec.Picture as P

import Control.Moffy.Viewable.Basic
import Control.Moffy.Viewable.Text
import Control.Moffy.Viewable.Shape
import Control.Moffy.Viewable.Image

import Field (
	Field, Pixel, Position, drawLine, drawStr, drawImage, textExtents )

---------------------------------------------------------------------------

-- * VIEW
-- * DRAW IMAGE PIXEL

---------------------------------------------------------------------------
-- VIEW
---------------------------------------------------------------------------

viewText :: Field -> VText -> IO ()
viewText f (Text'
	(colorToPixel -> p) fn fs
	(x, y)
	(T.unpack -> s)) = do
		XGlyphInfo _ _ dx dy _ _ <- textExtents f fn fs s
		drawStr f p fn fs (round x + fromIntegral dx) (round y + fromIntegral dy) s

viewLine :: Field -> Line -> IO ()
viewLine f (Line'
	(colorToPixel -> p) (round -> lw)
	(round -> x1, round -> y1)
	(round -> x2, round -> y2)) = drawLine f p lw x1 y1 x2 y2

viewImage :: Field -> Image -> IO ()
viewImage f (Image'
	(round -> x, round -> y) img) = case decodePng img of
		Left em -> putStrLn ("error: " ++ em) >> pure ()
		Right i -> drawImagePixel f i x y

decodePng :: Png -> Either String (P.Image P.PixelRGBA8)
decodePng p = ($ P.decodeImage (pngData p)) $ fmap
	(scaleBilinear (round $ pngWidth p) (round $ pngHeight p) . P.convertRGBA8)

colorToPixel :: Color -> Pixel
colorToPixel Color {
	colorRed = fromIntegral -> r,
	colorGreen = fromIntegral -> g,
	colorBlue = fromIntegral -> b } = r `shiftL` 16 .|. g `shiftL` 8 .|. b

---------------------------------------------------------------------------
-- DRAW IMAGE PIXEL
---------------------------------------------------------------------------

drawImagePixel :: Field -> P.Image PixelRGBA8 -> Field.Position -> Field.Position -> IO ()
drawImagePixel f img x y = drawImage f dt x y w h
	where
	dt = modify swap02s $ imageData img
	w = fromIntegral $ imageWidth img
	h = fromIntegral $ imageHeight img

swap02s :: Storable a => MVector s a -> ST s ()
swap02s v = zipWithM_ (MV.swap v) [0, 4 .. MV.length v] [2, 6 .. MV.length v]
