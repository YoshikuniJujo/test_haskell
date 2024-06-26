{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Char
import Data.CairoImage.Internal
import Data.JuicyCairo
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations as V

import qualified Data.Text as T

main :: IO ()
main = getArgs >>= \case
	wdth : slnt : _	| all ((||) <$> isDigit <*> (== '.')) wdth &&
			all ((||) <$> ((||) <$> isDigit <*> (== '.')) <*> (== '-')) slnt -> do
		s <- cairoImageSurfaceCreate CairoFormatArgb32 300 400
		cr <- cairoCreate s

		fd <- pangoFontDescriptionNew

		pangoFontDescriptionSet fd $ Family "Soulcraft"
		pangoFontDescriptionSet fd $ Size 20
		pangoFontDescriptionSetAxis fd $ Weight 500
		pangoFontDescriptionSetAxis fd . V.Width $ read wdth
		pangoFontDescriptionSetAxis fd . Slant $ read slnt

		fd' <- pangoFontDescriptionFreeze fd

		print $ pangoFontDescriptionGetAxis @V.Width fd'
		print $ pangoFontDescriptionGetAxis @Slant fd'

		pl <- pangoCairoCreateLayout cr
		pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
		pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!"
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-soulcraft.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "need width (0 - 100) and need slant (-90 - 90)"
