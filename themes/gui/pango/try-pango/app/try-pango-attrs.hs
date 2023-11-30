{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.TextAttributes
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.VerticalText
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = do
	print $ pangoColorParse "chocolate"
	print $ pangoColorParse "#0f0"
	print $ pangoColorParse "#00ff00"
	print $ pangoColorParse "#000fff000"
	print $ pangoColorParse "#0000ffff0000"
	putStrLn . maybe "bad color" pangoColorToString $ pangoColorParse "ivory"
	s <- cairoImageSurfaceCreate CairoFormatArgb32 400 600
	cr <- cairoCreate s

	at <- pangoAttrNew $ Size 20
	at' <- pangoAttrNew $ Size 25

	al <- pangoTextAttrListNew "Hello, world!\nこんにちは、世界!"
	print at
	print al
	pangoTextAttrListInsert al at 0 10
	pangoTextAttrListInsert al at' 15 maxBound
	al' <- pangoTextAttrListFreeze al

	pl <- pangoCairoCreateLayout cr

--	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!"
	pangoLayoutSet pl al'
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al2 <- pangoTextAttrListNew "源ノ角ゴシック 思源黑體 思源黑体"
--	at2 <- pangoAttrNew $ Family "Source Han Sans VF"
	at3 <- pangoAttrNew $ PangoLanguage "ja-jp"
--	pangoAttrListInsert al2 at2
	pangoTextAttrListInsert al2 at3 0 maxBound
	al2' <- pangoTextAttrListFreeze al2

	cairoMoveTo cr 0 70
	pangoLayoutSet pl al2'
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	at4 <- pangoAttrNew $ PangoLanguage "zh-tw"
	pangoTextAttrListInsert al2 at4 0 maxBound

	cairoMoveTo cr 0 90
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al2
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al3 <- pangoTextAttrListNew "Hello, world! こんにちは、世界!"
	at5 <- pangoAttrNew PangoStyleOblique
	at6 <- pangoAttrNew PangoStyleItalic
	pangoTextAttrListInsert al3 at5 0 5
	pangoTextAttrListInsert al3 at6 10 maxBound

	cairoMoveTo cr 0 110
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al3
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al4 <- pangoTextAttrListNew "華華華華華華華華華華華華華! こんにちは、世界!"
	at7 <- pangoAttrNew $ Family "Source Han Sans VF"
	pangoTextAttrListInsert al4 at7 0 maxBound
	applyInOrder' al4 $ (`zip` [1 .. ]) [
		PangoWeightThin, PangoWeightUltralight, PangoWeightLight,
		PangoWeightSemilight, PangoWeightBook, PangoWeightNormal,
		PangoWeightMedium, PangoWeightSemibold, PangoWeightBold,
		PangoWeightUltrabold, PangoWeightHeavy, PangoWeightUltraheavy ]

	cairoMoveTo cr 0 130
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al4
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	fd <- pangoFontDescriptionNew
--	pangoFontDescriptionSet fd $ Family "Source Han Sans VF"
	pangoFontDescriptionSet fd $ PangoGravityEast
	al5 <- pangoTextAttrListNew "あいうえおabcdefg"
	at8 <- pangoAttrFontDescNew fd
	pangoTextAttrListInsert al5 at8 0 8

	(\a -> pangoTextAttrListInsert al5 a 5 maxBound)
		=<< pangoAttrNew PangoGravityNorth

	cairoMoveTo cr 0 150
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al5
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al6 <- pangoTextAttrListNew . pangoLanguageGetSampleString =<< pangoLanguageGetDefault
	at9 <- pangoAttrNew . ForegroundColor $ RgbWord16 @Double 0 (maxBound `div` 2) 0
	at10 <- pangoAttrNew . BackgroundColor $ RgbWord16 @Double 0 0 maxBound
	pangoTextAttrListInsert al6 at9 0 15
	pangoTextAttrListInsert al6 at10 10 maxBound

	cairoMoveTo cr 0 170
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al6
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.2 0.5 0.1

	al7 <- pangoTextAttrListNew . pangoLanguageGetSampleString =<< pangoLanguageGetDefault
	at11 <- pangoAttrNew $ Strikethrough True
	at12 <- pangoAttrNew . StrikethroughColor $ RgbWord16 @Double 0 maxBound 0
	pangoTextAttrListInsert al7 at11 0 maxBound
	pangoTextAttrListInsert al7 at12 10 maxBound

	cairoMoveTo cr 0 190
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al7
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al8 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	applyInOrder' al8 $ (`zip` [6, 12 .. ]) [
		PangoUnderlineNone, PangoUnderlineSingle, PangoUnderlineDouble,
		PangoUnderlineLow, PangoUnderlineError ]
	applyInOrder' al8 $ (`zip` [7, 14 ..]) [
		UnderlineColor @Double $ RgbWord16 0 0 0,
		UnderlineColor $ RgbWord16 0 0 maxBound,
		UnderlineColor $ RgbWord16 0 (maxBound `div` 3 * 2) 0,
		UnderlineColor $ RgbWord16 maxBound 0 0 ]

	cairoMoveTo cr 0 210
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al8
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al9 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	at13 <- pangoAttrNew $ Shape
		(PangoRectangleFixed 5 3 16 16)
		(PangoRectangleFixed 0 0 19 19)
--	at13 <- pangoAttrNew $ Shape (PangoRectangle 5 0 30 30) (PangoRectangle 5 0 35 35)
	pangoTextAttrListInsert al9 at13 5 8

	cairoMoveTo cr 0 230
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al9
	print . pangoLayoutInfo @Extents =<< pangoLayoutFreeze pl
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al10 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	applyInOrder' al10 $ zip (Scale <$> [0.7, 0.8 .. 2.4] <> [2.3, 2.2 ..]) [1, 2 .. 42]

	cairoMoveTo cr 0 240
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al10
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al11 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	applyInOrder' al11 $ zip (Rise <$> [0, 0.5 .. 7.5] <> [7.0, 6.5 .. ]) [1 .. 42]

	cairoMoveTo cr 0 280
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al11
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al12 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	applyInOrder' al12 $ zip (LetterSpacing <$> [0, 0.5 .. 7.5] <> [7.0, 6.5 .. ]) [1 .. 42]

	cairoMoveTo cr 0 310
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al12
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al13 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	applyInOrder' al13 $ (`zip` [5, 10 .. 42]) [
		PangoGravitySouth, PangoGravityEast, PangoGravityNorth,
		PangoGravityWest ] -- PangoGravityAuto ]

	cairoMoveTo cr 0 330
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al13
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

--	al14 <- pangoAttrListThaw =<< pangoAttrListFreeze al13
--	pangoAttrListInsert al14 =<< pangoAttrNew PangoGravityHintNatural
--	pangoAttrListInsert al14 =<< pangoAttrNew PangoGravityHintLine
--	pangoAttrListInsertBefore al14 =<< pangoAttrNew PangoGravityHintStrong

	al14 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	(\a -> pangoTextAttrListInsert al14 a 0 maxBound) =<< pangoAttrNew (Family "Source Han Sans VF")
	(\a -> pangoTextAttrListInsert al14 a 3 maxBound) =<< pangoAttrNew PangoGravityEast

	{-
	applyInOrder al14 $ (`zip` [6, 12 ..]) [
		PangoGravitySouth, PangoGravityEast, PangoGravityNorth,
		PangoGravityWest ] -- PangoGravityAuto ]
		-}
	{-
	pangoAttrListInsert al14 =<< pangoAttrNew PangoGravityHintLine
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew PangoGravitySouth
		a <$ do	pangoAttributeSetStartIndex a 0
			pangoAttributeSetEndIndex a 5
	pangoAttrListInsert al14 =<< pangoAttrNew PangoGravityHintLine
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew PangoGravityEast
		a <$ do	pangoAttributeSetStartIndex a 5
			pangoAttributeSetEndIndex a 10
	pangoAttrListInsert al14 =<< pangoAttrNew PangoGravityHintLine
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew PangoGravityNorth
		a <$ do	pangoAttributeSetStartIndex a 10
			pangoAttributeSetEndIndex a 15
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew PangoGravityHintStrong
		pure a
--		a <$ do	pangoAttributeSetStartIndex a 15
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew PangoGravityNorth
		pure a
		a <$ do	pangoAttributeSetStartIndex a 1
--			pangoAttributeSetEndIndex a 20
	pangoAttrListInsert al14 =<< pangoAttrNew PangoGravityHintNatural
			-}

	cairoMoveTo cr 0 370
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al14
	pangoLayoutSet @T.Text pl "あいうえおかきくけこabcdefg"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al15 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	applyInOrder' al15 $ zip
		(ForegroundAlpha . AlphaWord16 @Double
			<$> [maxBound `div` 42, maxBound `div` 21 .. ])
		[1 .. 42]

	cairoMoveTo cr 0 400
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al15
	pangoLayoutSet pl . pangoLanguageGetSampleString $ PangoLanguage "en"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	al16 <- pangoTextAttrListNew . pangoLanguageGetSampleString $ PangoLanguage "en"
	(\a -> pangoTextAttrListChange al16 a 0 maxBound) =<< pangoAttrNew
		(BackgroundColor $ RgbWord16 @Double 0 (maxBound `div` 2) 0)
	applyInOrder' al16 $ zip
		(BackgroundAlpha . AlphaWord16 @Double
			<$> [maxBound `div` 42, maxBound `div` 21 .. ])
		[1 .. 42]

	cairoMoveTo cr 0 430
	pangoLayoutSet pl =<< pangoTextAttrListFreeze al16
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	tal <- pangoTextAttrListNew "Hello"
	attr <- pangoAttrNew $ Size 20
	pangoTextAttrListInsert tal attr 0 2
	pangoLayoutSet pl =<< pangoTextAttrListFreeze tal

	cairoMoveTo cr 0 450
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	tal' <- pangoTextAttrListNew "あいうえお"
	attr' <- pangoAttrNew $ Size 20
	pangoTextAttrListInsert tal' attr' 0 2
	pangoLayoutSet pl =<< pangoTextAttrListFreeze tal'

	cairoMoveTo cr 0 470
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-attrs.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

applyInOrder' :: (PangoAttributeValue v, PrimMonad m) =>
	PangoTextAttrListPrim (PrimState m) -> [(v, Int)] -> m ()
applyInOrder' tal vs = for_ vs \(w, i) -> do
	lat <- pangoAttrNew w
	pangoTextAttrListInsertBefore tal lat 0 i
