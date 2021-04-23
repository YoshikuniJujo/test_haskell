{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad.Primitive
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

import Graphics.Pango.Types
import Graphics.Pango.Values
import Graphics.Pango.Basic.TextAttributes
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 400 600
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
	pangoLayoutSet pl al'
	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!"
	pangoCairoShowLayout cr pl

	al2 <- pangoAttrListNew
--	at2 <- pangoAttrNew $ Family "Source Han Sans VF"
	at3 <- pangoAttrNew $ pangoLanguageFromString "ja-jp"
--	pangoAttrListInsert al2 at2
	pangoAttrListInsert al2 at3
	al2' <- pangoAttrListFreeze al2

	cairoMoveTo cr 0 70
	pangoLayoutSet pl al2'
	pangoLayoutSet @T.Text pl "源ノ角ゴシック 思源黑體 思源黑体"
	pangoCairoShowLayout cr pl

	at4 <- pangoAttrNew $ pangoLanguageFromString "zh-tw"
	pangoAttrListInsert al2 at4

	cairoMoveTo cr 0 90
	pangoLayoutSet pl =<< pangoAttrListFreeze al2
	pangoCairoShowLayout cr pl

	al3 <- pangoAttrListNew
	at5 <- pangoAttrNew pangoStyleOblique
	at6 <- pangoAttrNew pangoStyleItalic
	pangoAttributeSetEndIndex at5 5
	pangoAttributeSetStartIndex at6 10
	pangoAttrListInsert al3 at5
	pangoAttrListInsert al3 at6

	cairoMoveTo cr 0 110
	pangoLayoutSet pl =<< pangoAttrListFreeze al3
	pangoLayoutSet @T.Text pl "Hello, world! こんにちは、世界!"
	pangoCairoShowLayout cr pl

	al4 <- pangoAttrListNew
	at7 <- pangoAttrNew $ Family "Source Han Sans VF"
	pangoAttrListInsert al4 at7
	applyInOrder al4 $ (`zip` [3, 6 .. ]) [
		pangoWeightThin, pangoWeightUltralight, pangoWeightLight,
		pangoWeightSemilight, pangoWeightBook, pangoWeightNormal,
		pangoWeightMedium, pangoWeightSemibold, pangoWeightBold,
		pangoWeightUltrabold, pangoWeightHeavy, pangoWeightUltraheavy ]

	cairoMoveTo cr 0 130
	pangoLayoutSet pl =<< pangoAttrListFreeze al4
	pangoLayoutSet @T.Text pl "華華華華華華華華華華華華! こんにちは、世界!"
	pangoCairoShowLayout cr pl

	fd <- pangoFontDescriptionNew
--	pangoFontDescriptionSet fd $ Family "Source Han Sans VF"
	pangoFontDescriptionSet fd $ pangoGravityEast
	al5 <- pangoAttrListNew
	at8 <- pangoAttrNew =<< pangoFontDescriptionFreeze fd
	pangoAttributeSetEndIndex at8 18
	pangoAttrListInsert al5 at8

	pangoAttrListInsert al5 =<< do
		a <- pangoAttrNew pangoGravityNorth
		a <$ pangoAttributeSetStartIndex a 15

	cairoMoveTo cr 0 150
	pangoLayoutSet pl =<< pangoAttrListFreeze al5
	pangoLayoutSet @T.Text pl "あいうえおabcdefg"
	pangoCairoShowLayout cr pl

	al6 <- pangoAttrListNew
	at9 <- pangoAttrNew $ ForegroundColor 0 (maxBound `div` 2) 0
	pangoAttributeSetEndIndex at9 (15 * 3)
	at10 <- pangoAttrNew $ BackgroundColor 0 0 maxBound
	pangoAttributeSetStartIndex at10 (10 * 3)
	pangoAttrListInsert al6 at9
	pangoAttrListInsert al6 at10

	cairoMoveTo cr 0 170
	pangoLayoutSet pl =<< pangoAttrListFreeze al6
	pangoLayoutSet pl . T.pack . pangoLanguageGetSampleString =<< pangoLanguageGetDefault
	pangoCairoShowLayout cr pl

	al7 <- pangoAttrListNew
	at11 <- pangoAttrNew $ Strikethrough True
	at12 <- pangoAttrNew $ StrikethroughColor maxBound 0 0
	pangoAttrListInsert al7 at11
	pangoAttrListInsert al7 at12

	cairoMoveTo cr 0 190
	pangoLayoutSet pl =<< pangoAttrListFreeze al7
	pangoCairoShowLayout cr pl

	al8 <- pangoAttrListNew
	applyInOrder al8 $ (`zip` [6, 12 .. ]) [
		PangoUnderlineNone, PangoUnderlineSingle, PangoUnderlineDouble,
		PangoUnderlineLow, PangoUnderlineError ]
	applyInOrder al8 $ (`zip` [7, 14 ..]) [
		UnderlineColor 0 0 0, UnderlineColor 0 0 maxBound,
		UnderlineColor 0 (maxBound `div` 3 * 2) 0, UnderlineColor maxBound 0 0 ]

	cairoMoveTo cr 0 210
	pangoLayoutSet pl =<< pangoAttrListFreeze al8
	pangoLayoutSet pl . T.pack . pangoLanguageGetSampleString $ pangoLanguageFromString "en"
	pangoCairoShowLayout cr pl

	al9 <- pangoAttrListNew
	at13 <- pangoAttrNew $ Shape
		(PangoRectangle 5 3072 16384 16384)
		(PangoRectangle 0 0 19456 19456)
--	at13 <- pangoAttrNew $ Shape (PangoRectangle 5 0 30 30) (PangoRectangle 5 0 35 35)
	pangoAttributeSetStartIndex at13 5
	pangoAttributeSetEndIndex at13 8
	pangoAttrListInsert al9 at13

	cairoMoveTo cr 0 230
	pangoLayoutSet pl =<< pangoAttrListFreeze al9
	print =<< pangoLayoutGetExtents pl
	pangoCairoShowLayout cr pl

	al10 <- pangoAttrListNew
	applyInOrder al10 $ zip (Scale <$> [0.7, 0.8 .. 2.4] <> [2.3, 2.2 ..]) [1, 2 .. 42]

	cairoMoveTo cr 0 240
	pangoLayoutSet pl =<< pangoAttrListFreeze al10
	pangoCairoShowLayout cr pl

	al11 <- pangoAttrListNew
	applyInOrder al11 $ zip (Rise <$> [0, 0.5 .. 7.5] <> [7.0, 6.5 .. ]) [1 .. 42]

	cairoMoveTo cr 0 280
	pangoLayoutSet pl =<< pangoAttrListFreeze al11
	pangoCairoShowLayout cr pl

	al12 <- pangoAttrListNew
	applyInOrder al12 $ zip (LetterSpacing <$> [0, 0.5 .. 7.5] <> [7.0, 6.5 .. ]) [1 .. 42]

	cairoMoveTo cr 0 310
	pangoLayoutSet pl =<< pangoAttrListFreeze al12
	pangoCairoShowLayout cr pl

	al13 <- pangoAttrListNew
	applyInOrder al13 $ (`zip` [5, 10 .. 42]) [
		pangoGravitySouth, pangoGravityEast, pangoGravityNorth,
		pangoGravityWest ] -- pangoGravityAuto ]

	cairoMoveTo cr 0 330
	pangoLayoutSet pl =<< pangoAttrListFreeze al13
	pangoCairoShowLayout cr pl

--	al14 <- pangoAttrListThaw =<< pangoAttrListFreeze al13
--	pangoAttrListInsert al14 =<< pangoAttrNew pangoGravityHintNatural
--	pangoAttrListInsert al14 =<< pangoAttrNew pangoGravityHintLine
--	pangoAttrListInsertBefore al14 =<< pangoAttrNew pangoGravityHintStrong

	al14 <- pangoAttrListNew
	pangoAttrListInsert al14 =<< pangoAttrNew (Family "Source Han Sans VF")
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew pangoGravityEast
		a <$ pangoAttributeSetStartIndex a 3
	{-
	applyInOrder al14 $ (`zip` [6, 12 ..]) [
		pangoGravitySouth, pangoGravityEast, pangoGravityNorth,
		pangoGravityWest ] -- pangoGravityAuto ]
		-}
	{-
	pangoAttrListInsert al14 =<< pangoAttrNew pangoGravityHintLine
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew pangoGravitySouth
		a <$ do	pangoAttributeSetStartIndex a 0
			pangoAttributeSetEndIndex a 5
	pangoAttrListInsert al14 =<< pangoAttrNew pangoGravityHintLine
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew pangoGravityEast
		a <$ do	pangoAttributeSetStartIndex a 5
			pangoAttributeSetEndIndex a 10
	pangoAttrListInsert al14 =<< pangoAttrNew pangoGravityHintLine
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew pangoGravityNorth
		a <$ do	pangoAttributeSetStartIndex a 10
			pangoAttributeSetEndIndex a 15
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew pangoGravityHintStrong
		pure a
--		a <$ do	pangoAttributeSetStartIndex a 15
	pangoAttrListInsert al14 =<< do
		a <- pangoAttrNew pangoGravityNorth
		pure a
		a <$ do	pangoAttributeSetStartIndex a 1
--			pangoAttributeSetEndIndex a 20
	pangoAttrListInsert al14 =<< pangoAttrNew pangoGravityHintNatural
			-}

	cairoMoveTo cr 0 370
	pangoLayoutSet pl =<< pangoAttrListFreeze al14
	pangoLayoutSet @T.Text pl "あいうえおかきくけこabcdefg"
	pangoCairoShowLayout cr pl

	al15 <- pangoAttrListNew
	applyInOrder al15 $ zip (ForegroundAlpha <$> [maxBound `div` 42, maxBound `div` 21 .. ]) [1 .. 42]

	cairoMoveTo cr 0 400
	pangoLayoutSet pl =<< pangoAttrListFreeze al15
	pangoLayoutSet pl . T.pack . pangoLanguageGetSampleString $ pangoLanguageFromString "en"
	pangoCairoShowLayout cr pl

	al16 <- pangoAttrListNew
	pangoAttrListInsert al16 =<< pangoAttrNew (BackgroundColor 0 (maxBound `div` 2) 0)
	applyInOrder al16 $ zip (BackgroundAlpha <$> [maxBound `div` 42, maxBound `div` 21 .. ]) [1 .. 42]

	cairoMoveTo cr 0 430
	pangoLayoutSet pl =<< pangoAttrListFreeze al16
	pangoCairoShowLayout cr pl

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-attrs.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

applyInOrder :: (PangoAttributeValue v, PrimMonad m) =>
	PangoAttrListPrim (PrimState m) -> [(v, CUInt)] -> m ()
applyInOrder al vs = for_ vs \(w, i) -> do
	lat <- pangoAttrNew w
	pangoAttributeSetEndIndex lat i
	pangoAttrListInsertBefore al lat
