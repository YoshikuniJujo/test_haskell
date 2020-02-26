{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module View where

import Foreign.C.Types

import qualified Data.Text as T
import qualified Codec.Picture as JP

import Check.DrawDownloadImage

import qualified Field as F

type View = [View1]

data View1
	= Text FontSize Position T.Text
	| Image Position (JP.Image JP.PixelRGBA8)
	| Line LineWeight Position Position

type FontSize = Double
type LineWeight = CInt
type Position = (F.Position, F.Position)

view :: F.Field -> View -> IO ()
view f v = do
	F.clearField f
	view1 f `mapM_` v
	F.flushField f

view1 :: F.Field -> View1 -> IO ()
view1 f (Text fs (x, y) t) = F.drawStr f "sans" fs x y $ T.unpack t
view1 f (Image (x, y) img) = drawImage f img x y
view1 f (Line lw (xs, ys) (xe, ye)) = F.drawLine f 0xffffff lw xs ys xe ye
