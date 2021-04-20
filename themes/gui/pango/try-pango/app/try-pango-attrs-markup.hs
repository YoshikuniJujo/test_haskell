{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import System.Environment
import System.Glib.ErrorReporting
import Codec.Picture
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Pango.Basic.TextAttributes
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

sample :: T.Text
sample = "<big><i>Hello</i>, <b>world</b>!</big>"

main :: IO ()
main = do
	mu <- (<$> getArgs) \case
		[] -> sample
		[arg] -> T.pack arg
		_ -> error "bad"
	case pangoParseMarkup mu Nothing of
		Left e -> putStrLn =<< gErrorReport e
		Right (pal, t, _) -> do
			s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
			cr <- cairoCreate s

			pl <- pangoCairoCreateLayout cr
			pangoLayoutSetAttributes pl pal
			pangoLayoutSet pl t
			pangoCairoShowLayout cr pl

			cairoImageSurfaceGetCairoImage s >>= \case
				CairoImageArgb32 a -> writePng "try-pango-attrs.png" $ cairoArgb32ToJuicyRGBA8 a
				_ -> error "never occur"
