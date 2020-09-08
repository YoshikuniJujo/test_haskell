{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.ViewType (
	-- * VIEW
	View(..), View1(..),
	-- * COLOR
	Color(..), white, blue, Png(..) ) where

import Control.Moffy.Event.CalcTextExtents (FontName, FontSize)
import Data.Word (Word8)
import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Trial.Followbox.TypeSynonym (Position, LineWidth)

import Control.Moffy.Handle.GtkField
import Graphics.Gtk.Cairo
import Graphics.Gtk.Pango

---------------------------------------------------------------------------

newtype View = View [View1] deriving Show

instance Semigroup View where
	View vs1 <> View vs2 = View $ vs1 <> vs2

instance Monoid View where
	mempty = View []

data View1
	= Text Color FontName FontSize Position Text
	| Line Color LineWidth Position Position
	| Image Position Png

instance Show View1 where
	show (Text c fn fs p t) = "Text " ++ show c ++ " " ++ show fn ++ " " ++ show fs ++ " " ++ show p ++ " " ++ show t
	show (Line c lw s e) = "Line " ++ show c ++ " " ++ show lw ++ " " ++ show s ++ " " ++ show e
	show (Image p _) = "Image " ++ show p ++ " <Image PixelRGBA8>"

data Color =
	Color { colorRed :: Word8, colorGreen :: Word8, colorBlue :: Word8 }
	deriving Show

white, blue :: Color
white = Color { colorRed = 0xff, colorGreen = 0xff, colorBlue = 0xff }
blue = Color { colorRed = 0x30, colorGreen = 0x66, colorBlue = 0xd6 }

data Png = Png { pngWidth :: Int, pngHeight :: Int, pngData :: BS.ByteString }
	deriving Show

instance Drawable View where
	draw cr (View v) = draw cr v

instance Drawable View1 where
	draw cr (Text c fn fs (x, y) t) = do
		l <- pangoCairoCreateLayout cr
		d <- pangoFontDescriptionFromString $ T.pack fn
		pangoFontDescriptionSetAbsoluteSize d fs
		pangoLayoutSetFontDescription l d
		pangoLayoutSetText l t
		uncurry3 (cairoSetSourceRgb cr) $ colorToRgb c
		cairoMoveTo cr x y
		pangoCairoShowLayout cr l
	draw cr (Line c w (xb, yb) (xe, ye)) = do
		uncurry3 (cairoSetSourceRgb cr) $ colorToRgb c
		cairoSetLineWidth cr w
		cairoMoveTo cr xb yb
		cairoLineTo cr xe ye
		cairoStroke cr
	draw cr (Image _ _) = pure ()

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

colorToRgb :: Color -> (Double, Double, Double)
colorToRgb (Color (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b)) =
	(r / 0xff, g / 0xff, b / 0xff)
