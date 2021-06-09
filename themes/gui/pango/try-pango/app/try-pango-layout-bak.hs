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
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine
import Graphics.Pango.Basic.TabStops
import Graphics.Pango.Rendering.Cairo

import Data.Color
import Data.CairoImage
import Data.JuicyCairo

import qualified Data.Text as T

pangoScale :: Num n => n
pangoScale = fromIntegral $ resolution @Type @PU undefined

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 900 900
	cr <- cairoCreate s
	pl2 <- pangoCairoCreateLayout cr
	pangoLayoutSet pl2 someText
	fpl2 <- pangoLayoutFreeze pl2

	itr <- pangoLayoutGetIter fpl2
	pangoCairoShowGlyphItem cr someText . fromJust =<< pangoLayoutIterGetRun itr
	pangoLayoutIterFree itr

	itr2 <- pangoLayoutGetIter fpl2
	pangoCairoShowGlyphItem cr someText . fromJust =<< pangoLayoutIterGetRun itr2

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-layout.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
	print itr
	print itr2

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

tabArrayFixed :: [PangoFixed] -> PangoTabArray
tabArrayFixed ps = runST do
	pta <- pangoTabArrayFixedNew
	for_ (zip [0 ..] ps) \(i, p) -> pangoTabArrayFixedSetTab pta i p
	pangoTabArrayFixedFreeze pta

tabArrayInt :: [CInt] -> PangoTabArray
tabArrayInt ps = runST do
	pta <- pangoTabArrayIntNew
	for_ (zip [0 ..] ps) \(i, p) -> pangoTabArrayIntSetTab pta i p
	pangoTabArrayIntFreeze pta
