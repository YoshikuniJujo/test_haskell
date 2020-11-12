{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Text.Nowdoc
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Paths
import Graphics.Cairo.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.Fonts
import Graphics.Pango.Basic.LayoutObjects
-- import Graphics.Pango.Rendering.Cairo
import Graphics.Pango.Types
import Graphics.Pango.Values

import Lib

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 700 800
	cr <- cairoCreate s

	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
--	pangoFontDescriptionSetFamily pfd "sans-serif"
	pangoFontDescriptionSetFamily pfd "sans-serif"
	pangoFontDescriptionSetSize pfd (30 * pangoScale)
	pfd' <- pangoFontDescriptionFreeze pfd
	pangoLayoutSetFontDescription pl pfd'
	pangoLayoutSetWidth pl (200 * pangoScale)
	pangoLayoutSetEllipsize pl pangoEllipsizeMiddle
--	pangoLayoutSetText pl "こんにちは世界!" 100
	pangoLayoutSetText pl "Hello, world!\nこんにちは世界!" 100
--	pangoLayoutSetText pl "Hello, world!\x2026\x22ef\nこんにちは世界!\x2026\x22ef" 100
--	pangoLayoutSetText pl "Hello, world!\x22ef\x2026\x22ef" 100
	cairoMoveTo cr 100 100
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	pl2 <- pangoCairoCreateLayout cr
	pfd2 <- pangoFontDescriptionNew
	pangoFontDescriptionSetFamily pfd2 "serif"
	pangoFontDescriptionSetSize pfd2 (15 * pangoScale)
	pangoLayoutSetFontDescription pl2 =<< pangoFontDescriptionFreeze pfd2
	pangoLayoutSetWidth pl2 (400 * pangoScale)
	pangoLayoutSetIndent pl2 (30 * pangoScale)
--	pangoLayoutSetLineSpacing pl2 2
	pangoLayoutSetAlignment pl2 pangoAlignCenter
	pangoLayoutSetText pl2 someText 1600
	cairoMoveTo cr 100 200
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl2

{-
	pl3 <- pangoCairoCreateLayout cr
	pfd3 <- pangoFontDescriptionNew
	pangoLayoutSetAlignment pl pangoAlignCenter
	cairoMoveTo cr 100 400
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl3
	-}

	void $ writeDynamicPng "tmp3.png" =<< cairoImageSurfaceGetImage s

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
