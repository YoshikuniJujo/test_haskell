{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
	cr <- cairoCreate s

	putStrLn . pangoLanguageGetSampleString =<< pangoLanguageGetDefault
	putStrLn . pangoLanguageGetSampleString $ pangoLanguageFromString "is-is"
	putStrLn . pangoLanguageGetSampleString $ pangoLanguageFromString "zh-tw"
	putStrLn sampleText

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSet pl $ Width 200
	pangoLayoutSet pl $ T.pack sampleText
	pangoCairoShowLayout cr pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-layout-innocuous.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

sampleText :: String
sampleText = unwords $
	pangoLanguageGetSampleString . pangoLanguageFromString <$> [
		"is-is", "is", "ga-ie", "ga", "en", "ja-jp", "zh-tw"
		]
