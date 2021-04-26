{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 600 800
	cr <- cairoCreate s

	putStrLn . pangoLanguageGetSampleString =<< pangoLanguageGetDefault
	putStrLn . pangoLanguageGetSampleString $ pangoLanguageFromString "is-is"
	putStrLn . pangoLanguageGetSampleString $ pangoLanguageFromString "zh-tw"
	putStrLn sampleText
	putStrLn sampleText2

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSet pl $ pangoEllipsizeMiddle
	pangoLayoutSet pl $ Width 180
	pangoLayoutSet pl $ LinesPerParagraph 3
	print =<< pangoLayoutGet @Width pl
	print =<< pangoLayoutGet @Height pl
	pangoLayoutSet pl . T.pack $ sampleText ++ "\n" ++ sampleText2
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 150
	pangoLayoutSet pl PangoWrapChar
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 300
	pangoLayoutSet pl PangoWrapWordChar
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 450
	pangoLayoutSet pl $ Width 30
	pangoLayoutSet pl PangoWrapWord
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 600
	pangoLayoutSet pl PangoWrapWordChar
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 300 0
	pangoLayoutSet pl $ Width 200
	pangoLayoutSet pl $ Indent 50
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 300 150
	pangoLayoutSet pl . Indent $ - 50
	pangoCairoShowLayout cr pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-layout-innocuous.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

sampleText, sampleText2 :: String
sampleText = unwords $
	pangoLanguageGetSampleString . pangoLanguageFromString <$> [
		"is-is", "ga-ie", "ga", "en", "ja-jp", "zh-tw"
		]

sampleText2 = unwords $
	pangoLanguageGetSampleString . pangoLanguageFromString <$> [
		"af", "ar", "sq"
		]
