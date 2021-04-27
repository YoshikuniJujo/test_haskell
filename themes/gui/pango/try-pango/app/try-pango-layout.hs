{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import Data.Int
import Text.Nowdoc
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.TabStops
import Graphics.Pango.Rendering.Cairo
import Graphics.Pango.Types
import Graphics.Pango.Values

import Data.Color
import Data.CairoImage
import Data.JuicyCairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 900 900
	cr <- cairoCreate s

	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd $ Family "sans-serif"
	pangoFontDescriptionSet pfd $ Size 30
	pangoLayoutSet pl =<< pangoFontDescriptionFreeze pfd
	print @(Maybe Family) . pangoFontDescriptionGet =<< pangoLayoutGet pl

	pangoLayoutSetWidth pl (200 * pangoScale)
	pangoLayoutSetEllipsize pl pangoEllipsizeMiddle
	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは世界!"
	cairoMoveTo cr 100 50
	pangoCairoShowLayout cr pl

	pl2 <- pangoCairoCreateLayout cr
	pfd2 <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd2 $ Family "serif"
	pangoFontDescriptionSet pfd2 $ Size 15
	pangoLayoutSet pl2 =<< pangoFontDescriptionFreeze pfd2
	pangoLayoutSetWidth pl2 (400 * pangoScale)
	pangoLayoutSetIndent pl2 (30 * pangoScale)
--	pangoLayoutSetLineSpacing pl2 2
	pangoLayoutSetAlignment pl2 pangoAlignCenter
	pangoLayoutSet pl2 $ T.pack someText
	cairoMoveTo cr 100 150
	let	fpl2 = pl2
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
	print =<< pangoLayoutMoveCursorVisually fpl2 True 0 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 1 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 2 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 3 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 6 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 7 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 8 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 9 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 10 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 11 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 12 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 13 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 100 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 101 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 102 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 103 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 104 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 105 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 106 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 107 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 108 0 0
	print =<< pangoLayoutMoveCursorVisually fpl2 True 109 0 0
	putStrLn "extents"
	print =<< pangoLayoutGetExtents fpl2
	print =<< pangoLayoutGetPixelExtents fpl2
	print =<< pangoLayoutGetPixelSize fpl2
	print =<< pangoLayoutGetBaseline fpl2
	print =<< pangoLayoutGetLineCount fpl2
	print =<< pangoLayoutLineGetExtents =<< pangoLayoutGetLine fpl2 2
	print =<< pangoLayoutLineGetPixelExtents =<< pangoLayoutGetLine fpl2 2
	print =<< mapM pangoLayoutLineGetPixelExtents =<< pangoLayoutGetLines fpl2
	pangoCairoShowLayout cr fpl2

	pl3 <- pangoCairoCreateLayout cr
	pangoLayoutSetAlignment pl3 pangoAlignCenter
	pangoLayoutSetTabs pl3 $ tabArray True [100, 200, 300, 400, 500, 600]
	pangoLayoutSet @T.Text pl3 "タブの\tテスト\tだよ\tHello,\tworld\t!"
	cairoMoveTo cr 100 580
	pangoCairoShowLayout cr pl3

	cairoMoveTo cr 100 600
	pangoLayoutSetTabs pl3 . tabArray False $ (* 1024) <$> [100, 210, 300, 400, 500, 600]
	pangoCairoShowLayout cr pl3

	pangoLayoutSetTabs pl3 $ tabArray False $ (* pangoScale) <$> [100, 200, 300, 350, 450, 550]
	cairoMoveTo cr 100 630
	pangoCairoShowLayout cr pl3

	pl4 <- pangoCairoCreateLayout cr
	pangoLayoutSet @T.Text pl4 "try\nsingle\tparagraph\nmode"
	pangoLayoutSetSingleParagraphMode pl4 True
	cairoMoveTo cr 100 655
	pangoCairoShowLayout cr pl4

	pl5 <- pangoCairoCreateLayout cr
	pangoLayoutSet @T.Text pl5 $
		"Watch \x231a Sloth \x1f9a5 Otter \x1f9a6 Secret \x3299 A \x1f170 " <>
		"Bad1 \x1f16f Bad2 \x1f16e fffi"
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 1
	cairoMoveTo cr 100 680
	let	fpl5 = pl5
	print =<< pangoLayoutGetUnknownGlyphsCount fpl5
	pangoCairoShowLayout cr fpl5

	itr <- pangoLayoutGetIter fpl2
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

	itr2 <- pangoLayoutGetIter fpl2
	print =<< pangoLayoutIterGetIndex itr2
	void $ pangoLayoutIterNextCluster itr2
	print =<< pangoLayoutIterGetIndex itr2
	void $ pangoLayoutIterNextLine itr2
	cairoMoveTo cr 100 770
	pangoCairoShowGlyphItem cr someText . fromJust =<< pangoLayoutIterGetRun itr2
	cairoMoveTo cr 100 820
	pll <- pangoLayoutIterGetLine itr2
	pangoCairoShowLayoutLine cr pll
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

someText :: String
someText = unlines [
	concat $ lines [nowdoc|
Haskellとの出会いは、
「Rubyソースコード完全解説」という書籍の、
「(Rubyのソースコードは)少なくともHaskellやPL/Iで
書いてあるわけではないので一般人にも読める可能性が高い」という、一文でした。
「『一般人に読め』ないHaskellとは、一体、何だ?」と興味を持ち、
「Haskell: The Craft of Functional Programming」という書籍を購入しました。
|],
	concat $ lines [nowdoc|
この本は洋書でしたが、実際に手を動かしながら、学んでいくことができたので、
楽しく読み進めることができました。
それまでにも、いくつかのプログラミング言語を学んだ経験があったのですが、
それらに感じていた不満が、ことごとく解消されていくのを感じました。
|]
	]

tabArray :: Bool -> [Int32] -> PangoTabArray
tabArray pip ps = runST do
	pta <- pangoTabArrayNew (fromIntegral $ length ps) pip
	for_ (zip [0 ..] ps) \(i, p) -> pangoTabArraySetTab pta i pangoTabLeft p
	pangoTabArrayFreeze pta
