{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module View where

import qualified Data.Text as T
import qualified Codec.Picture as JP

import Check.DrawDownloadImage

import qualified Field as F

type View n = [View1 n]

data View1 n
	= Text F.Pixel FontSize (Position n) T.Text
	| Image (Position n) (JP.Image JP.PixelRGBA8)
	| Line F.Pixel (LineWeight n) (Position n) (Position n)

type FontSize = Double
type LineWeight n = n
type Position n = (n, n)

view :: Integral n => F.Field -> View n -> IO ()
view f v = do
	F.clearField f
	view1 f `mapM_` v
	F.flushField f

view1 :: Integral n => F.Field -> View1 n -> IO ()
view1 f (Text c fs (x, y) t) =
	F.drawStr f c "sans" fs (fromIntegral x) (fromIntegral y) $ T.unpack t
view1 f (Image (x, y) img) = drawImage f img (fromIntegral x) (fromIntegral y)
view1 f (Line c lw (xs, ys) (xe, ye)) =
	F.drawLine f c (fromIntegral lw)
		(fromIntegral xs) (fromIntegral ys)
		(fromIntegral xe) (fromIntegral ye)
