{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad
import Control.Monad.ST
import Data.Kind
import Data.Foldable
import Data.Maybe
import Data.Fixed
import Text.Nowdoc
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine
import Graphics.Pango.Rendering.Cairo
import Graphics.Pango.LowLevel.TabStops

import Data.Color
import Data.CairoImage.Internal
import Data.JuicyCairo

import qualified Data.Text as T

pangoScale :: Num n => n
pangoScale = fromIntegral $ resolution @Type @PU undefined

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 900 900
	cr <- cairoCreate s

	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd $ Family "sans-serif"
	pangoFontDescriptionSet pfd $ Size 30
	pangoLayoutSet pl . pangoFontDescriptionToNullable . Just =<< pangoFontDescriptionFreeze pfd
	print @(Maybe Family) . pangoFontDescriptionGet . fromJust . pangoFontDescriptionFromNullable . pangoLayoutGet =<< pangoLayoutFreeze pl

	pangoLayoutSet pl . Width $ 200 * pangoScale
	pangoLayoutSet pl PangoEllipsizeMiddle
	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは世界!"
	cairoMoveTo cr 100 50
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	pl2 <- pangoCairoCreateLayout cr
	pfd2 <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd2 $ Family "serif"
	pangoFontDescriptionSet pfd2 $ Size 15
	pangoLayoutSet pl2 . pangoFontDescriptionToNullable . Just =<< pangoFontDescriptionFreeze pfd2
	pangoLayoutSet pl2 $ Width 400
	pangoLayoutSet pl2 $ Indent 30
--	pangoLayoutSetLineSpacing pl2 2
	pangoLayoutSet pl2 PangoAlignCenter
	pangoLayoutSet pl2 someText
	cairoMoveTo cr 100 150
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl2

	fpl2 <- pangoLayoutFreeze pl2
	putStrLn "0, 1, 5, 6"
	print =<< pangoLayoutIndexToPos fpl2 0
	print =<< pangoLayoutIndexToPos fpl2 1
	print =<< pangoLayoutIndexToPos fpl2 5
	print =<< pangoLayoutIndexToPos fpl2 6
	putStrLn "7, 8, 9, 10, 11, ..., 16"
	print =<< pangoLayoutIndexToPos fpl2 7
	print =<< pangoLayoutIndexToPos fpl2 8
	print =<< pangoLayoutIndexToPos fpl2 9
	print =<< pangoLayoutIndexToPos fpl2 10
	print =<< pangoLayoutIndexToPos fpl2 11
	print =<< pangoLayoutIndexToPos fpl2 12
	print =<< pangoLayoutIndexToPos fpl2 13
	print =<< pangoLayoutIndexToPos fpl2 14
	print =<< pangoLayoutIndexToPos fpl2 15
	print =<< pangoLayoutIndexToPos fpl2 16
	putStrLn "20, 21, 22, 23, ..., 30"
	print =<< pangoLayoutIndexToPos fpl2 20
	print =<< pangoLayoutIndexToPos fpl2 21
	print =<< pangoLayoutIndexToPos fpl2 22
	print =<< pangoLayoutIndexToPos fpl2 23
	print =<< pangoLayoutIndexToPos fpl2 24
	print =<< pangoLayoutIndexToPos fpl2 25
	print =<< pangoLayoutIndexToPos fpl2 26
	print =<< pangoLayoutIndexToPos fpl2 27
	print =<< pangoLayoutIndexToPos fpl2 28
	print =<< pangoLayoutIndexToPos fpl2 29
	print =<< pangoLayoutIndexToPos fpl2 30
	putStrLn "100, 101, 102, 103"
	print =<< pangoLayoutIndexToPos fpl2 100
	print =<< pangoLayoutGetCursorPos fpl2 100
	print =<< pangoLayoutIndexToPos fpl2 101
	print =<< pangoLayoutGetCursorPos fpl2 101
	print =<< pangoLayoutIndexToPos fpl2 102
	print =<< pangoLayoutGetCursorPos fpl2 102
	print =<< pangoLayoutIndexToPos fpl2 103
	print =<< pangoLayoutGetCursorPos fpl2 103
	putStrLn "0, 1, 2"
	print =<< pangoLayoutIndexToLineX fpl2 0 False
	print =<< pangoLayoutIndexToLineX fpl2 1 False
	print =<< pangoLayoutIndexToLineX fpl2 1 True
	print =<< pangoLayoutIndexToLineX fpl2 2 False
	putStrLn "100, 101, 102, 103"
	print =<< pangoLayoutIndexToLineX fpl2 100 False
	print =<< pangoLayoutIndexToLineX fpl2 101 False
	print =<< pangoLayoutIndexToLineX fpl2 102 False
	print =<< pangoLayoutIndexToLineX fpl2 103 False
	putStrLn "(50, 50), (100, 100), (500, 500)"
	print =<< pangoLayoutXyToIndex fpl2 (50 * pangoScale) (50 * pangoScale)
	print =<< pangoLayoutXyToIndex fpl2 (100 * pangoScale) (100 * pangoScale)
	print =<< pangoLayoutXyToIndex fpl2 (500 * pangoScale) (500 * pangoScale)
	print =<< pangoLayoutXyToIndex fpl2 345088 25600
	putStrLn "move cursor"
	print =<< pangoLayoutMoveCursorVisually fpl2 True 0 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 1 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 2 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 3 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 6 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 7 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 8 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 9 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 10 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 11 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 12 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 13 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 100 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 101 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 102 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 103 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 104 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 105 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 106 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 107 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 108 False R
	print =<< pangoLayoutMoveCursorVisually fpl2 True 109 False R
	putStrLn "extents"
	print $ pangoLayoutInfo @Extents fpl2
	putStrLn "pangoLayoutGetPixelExtents:"
	print $ pangoLayoutInfo @PixelExtents fpl2
	print $ pangoLayoutInfo @LayoutPixelSize fpl2
	putStrLn "pangoLayoutInfo @Baseline"
	print $ pangoLayoutInfo @Baseline fpl2
	print $ pangoLayoutInfo @LineCount fpl2
	print =<< pangoLayoutLineGetExtents . fromJust =<< pangoLayoutGetLine fpl2 2
	putStrLn "foo"
	print =<< pangoLayoutLineGetPixelExtents . fromJust =<< pangoLayoutGetLine fpl2 2
	putStrLn "bar"
	print =<< mapM pangoLayoutLineGetPixelExtents =<< pangoLayoutGetLines fpl2

	pangoCairoShowLayout cr fpl2

	pl3 <- pangoCairoCreateLayout cr
	pangoLayoutSet pl3 PangoAlignCenter
	pangoLayoutSet pl3 $ tabArrayInt [100, 200, 300, 400, 500, 600]
	pangoLayoutSet @T.Text pl3 "タブの\tテスト\tだよ\tHello,\tworld\t!"
	cairoMoveTo cr 100 540
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl3

	cairoMoveTo cr 100 560
	pangoLayoutSet pl3 . tabArrayFixed $ [100, 210, 300, 400, 500, 600]
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl3

	pangoLayoutSet pl3 $ tabArrayFixed $ [100, 200, 300, 350, 450, 490, 490, 490, 490]
	cairoMoveTo cr 100 580
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl3

	cairoMoveTo cr 100 600
	pangoLayoutSet @T.Text pl3
		"タブのテストテストテストテスト\tテスト\tだよ\tHello,\tworld\t!\t!\t!\t!\t!"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl3

	cairoMoveTo cr 100 620
--	pangoLayoutSet pl3 $ tabArrayInt [100, 200, 0, 0]
	pangoLayoutSet @T.Text pl3 "a\tb\tc\td\te\tf\tg"
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl3

	cairoMoveTo cr 100 640
	pangoLayoutSet pl3 $ tabArrayInt [100, 200, 300, 400]
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl3

	cairoMoveTo cr 100 660
	pangoLayoutSet pl3 $ tabArrayInt [100, 200]
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl3

	pl4 <- pangoCairoCreateLayout cr
	pangoLayoutSet @T.Text pl4 "try\nsingle\tparagraph\nmode"
	pangoLayoutSet pl4 $ SingleParagraphMode True
	cairoMoveTo cr 100 675
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl4

	pl5 <- pangoCairoCreateLayout cr
	pangoLayoutSet @T.Text pl5 $
		"Watch \x231a Sloth \x1f9a5 Otter \x1f9a6 Secret \x3299 A \x1f170 " <>
		"Bad1 \x1f16f Bad2 \x1f16e fffi"
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 1
	cairoMoveTo cr 100 700
	fpl5 <- pangoLayoutFreeze pl5
	print $ pangoLayoutInfo @UnknownGlyphsCount fpl5
	pangoCairoShowLayout cr fpl5

--	pangoLayoutWithIter fpl2 \itr -> do
	itr <- pangoLayoutGetIter fpl2
	do
		cairoMoveTo cr 100 740
		pangoCairoShowGlyphItem cr someText . fromJust =<< pangoLayoutIterGetRun itr
		void $ pangoLayoutIterNextRun itr
		void $ pangoLayoutIterNextChar itr
		print =<< pangoLayoutIterGetIndex itr
		void $ pangoLayoutIterNextChar itr
		print =<< pangoLayoutIterGetIndex itr
		cairoMoveTo cr 200 740
		pangoCairoShowGlyphItem cr someText . fromJust =<< pangoLayoutIterGetRun itr
		void $ pangoLayoutIterNextRun itr
		cairoMoveTo cr 300 740
		pangoCairoShowGlyphItem cr someText . fromJust =<< pangoLayoutIterGetRun itr

--	pangoLayoutWithIter fpl2 \itr2 -> do
	itr2 <- pangoLayoutGetIter fpl2
	do
		print =<< pangoLayoutIterGetIndex itr2
		void $ pangoLayoutIterNextCluster itr2
		print =<< pangoLayoutIterGetIndex itr2
		void $ pangoLayoutIterNextLine itr2
		cairoMoveTo cr 100 770
		pangoCairoShowGlyphItem cr someText . fromJust =<< pangoLayoutIterGetRun itr2
		cairoMoveTo cr 100 820
		pll <- pangoLayoutIterGetLine itr2
		pangoCairoShowLayoutLine cr pll
		putStrLn "pangoLayoutIterGetCharExtents:"
		print =<< pangoLayoutIterGetCharExtents itr2
		print =<< pangoLayoutIterGetClusterExtents itr2
		print =<< pangoLayoutIterGetRunExtents itr2
		print =<< pangoLayoutIterGetLineYrange itr2
		print =<< pangoLayoutIterGetLineExtents itr2
		print =<< pangoLayoutLineGetXRanges pll 10 100

--	void $ writeDynamicPng "tmp3.png" =<< cairoImageSurfaceGetImage s
	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-layout.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

someText :: T.Text
someText = T.unlines [
	T.concat $ T.lines [nowdoc|
Haskellとの出会いは、
「Rubyソースコード完全解説」という書籍の、
「(Rubyのソースコードは)少なくともHaskellやPL/Iで
書いてあるわけではないので一般人にも読める可能性が高い」という、一文でした。
「『一般人に読め』ないHaskellとは、一体、何だ?」と興味を持ち、
「Haskell: The Craft of Functional Programming」という書籍を購入しました。
|],
	T.concat $ T.lines [nowdoc|
この本は洋書でしたが、実際に手を動かしながら、学んでいくことができたので、
楽しく読み進めることができました。
それまでにも、いくつかのプログラミング言語を学んだ経験があったのですが、
それらに感じていた不満が、ことごとく解消されていくのを感じました。
|]
	]

tabArrayFixed :: [PangoFixed] -> PangoTabArrayNullable
tabArrayFixed ps = pangoTabArrayToNullable . Just $ runST do
	pta <- pangoTabArrayFixedNew
	for_ (zip [0 ..] ps) \(i, p) -> pangoTabArrayFixedSetTab pta i p
	pangoTabArrayFixedFreeze pta

tabArrayInt :: [CInt] -> PangoTabArrayNullable
tabArrayInt ps = pangoTabArrayToNullable . Just $ runST do
	pta <- pangoTabArrayIntNew
	for_ (zip [0 ..] ps) \(i, p) -> pangoTabArrayIntSetTab pta i p
	pangoTabArrayIntFreeze pta
