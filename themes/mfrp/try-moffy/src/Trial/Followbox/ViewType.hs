{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.ViewType (
	-- * VIEW
	View, View1(..),
	-- * COLOR
	Color(..), white, blue, Png(..) ) where

import Control.Moffy.Event.CalcTextExtents (FontName, FontSize)
import Data.Word (Word8)
import Codec.Picture (Image, PixelRGBA8)
import Data.Text (Text)

import qualified Data.ByteString as BS

import Trial.Followbox.TypeSynonym (Position, LineWidth)

---------------------------------------------------------------------------

type View = [View1]

data View1
	= Text Color FontName FontSize Position Text
	| Line Color LineWidth Position Position
	| Image Position (Image PixelRGBA8)

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
