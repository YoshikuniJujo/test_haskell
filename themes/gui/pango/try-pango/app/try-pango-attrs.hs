{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Foldable
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Values
import Graphics.Pango.Basic.TextAttributes
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 400 400
	cr <- cairoCreate s

	at <- pangoAttrNew $ Size 20
	at' <- pangoAttrNew $ Size 25
	al <- pangoAttrListNew
	print at
	print al
	pangoAttributeSetEndIndex at 10
	pangoAttributeSetStartIndex at' 15
	pangoAttrListInsert al at
	pangoAttrListInsert al at'
	al' <- pangoAttrListFreeze al

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSetAttributes pl al'
	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!"
	pangoCairoShowLayout cr pl

	al2 <- pangoAttrListNew
--	at2 <- pangoAttrNew $ Family "Source Han Sans VF"
	at3 <- pangoAttrNew $ pangoLanguageFromString "ja-jp"
--	pangoAttrListInsert al2 at2
	pangoAttrListInsert al2 at3
	al2' <- pangoAttrListFreeze al2

	cairoMoveTo cr 0 70
	pangoLayoutSetAttributes pl al2'
	pangoLayoutSet @T.Text pl "源ノ角ゴシック 思源黑體 思源黑体"
	pangoCairoShowLayout cr pl

	at4 <- pangoAttrNew $ pangoLanguageFromString "zh-tw"
	pangoAttrListInsert al2 at4

	cairoMoveTo cr 0 90
	pangoLayoutSetAttributes pl =<< pangoAttrListFreeze al2
	pangoCairoShowLayout cr pl

	al3 <- pangoAttrListNew
	at5 <- pangoAttrNew pangoStyleOblique
	at6 <- pangoAttrNew pangoStyleItalic
	pangoAttributeSetEndIndex at5 5
	pangoAttributeSetStartIndex at6 10
	pangoAttrListInsert al3 at5
	pangoAttrListInsert al3 at6

	cairoMoveTo cr 0 110
	pangoLayoutSetAttributes pl =<< pangoAttrListFreeze al3
	pangoLayoutSet @T.Text pl "Hello, world! こんにちは、世界!"
	pangoCairoShowLayout cr pl

	al4 <- pangoAttrListNew
	at7 <- pangoAttrNew $ Family "Source Han Sans VF"
	pangoAttrListInsert al4 at7
	for_ (zip [
		pangoWeightThin, pangoWeightUltralight, pangoWeightLight,
		pangoWeightSemilight, pangoWeightBook, pangoWeightNormal,
		pangoWeightMedium, pangoWeightSemibold, pangoWeightBold,
		pangoWeightUltrabold, pangoWeightHeavy, pangoWeightUltraheavy
		] [3, 6 .. ]) \(w, i) -> do
		lat <- pangoAttrNew w
		pangoAttributeSetEndIndex lat i
		pangoAttrListInsertBefore al4 lat

	cairoMoveTo cr 0 130
	pangoLayoutSetAttributes pl =<< pangoAttrListFreeze al4
	pangoLayoutSet @T.Text pl "華華華華華華華華華華華華! こんにちは、世界!"
	pangoCairoShowLayout cr pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-attrs.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
