{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Text.Nowdoc
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces

import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Data.CairoImage.Internal
import Data.JuicyCairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 900 900
	cr <- cairoCreate s

	pl2 <- pangoCairoCreateLayout cr
	pfd2 <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd2 $ Family "serif"
	pangoFontDescriptionSet pfd2 $ Size 15
	pangoLayoutSet pl2 . pangoFontDescriptionToNullable . Just =<< pangoFontDescriptionFreeze pfd2
	pangoLayoutSet pl2 $ Width 400
	pangoLayoutSet pl2 $ Indent 30
	pangoLayoutSet pl2 PangoAlignCenter
	pangoLayoutSet pl2 someText
	cairoMoveTo cr 100 150
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl2

	fpl2 <- pangoLayoutFreeze pl2
	putStrLn "extents"
	print =<< pangoLayoutInfo @Extents fpl2
	putStrLn "\npangoLayoutGetPixelExtents:"
	print =<< pangoLayoutInfo @PixelExtents fpl2
	putStrLn ""
	print =<< pangoLayoutInfo @LayoutPixelSize fpl2

	pangoCairoShowLayout cr fpl2

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "png/try-pango-layout-extent.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

	putStrLn "\nFOO"
	pl3 <- pangoCairoCreateLayout cr
	pfd3 <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd3 $ Family "serif"
	pangoFontDescriptionSet pfd3 $ AbsoluteSize 30
	pangoLayoutSet pl3 . pangoFontDescriptionToNullable . Just =<< pangoFontDescriptionFreeze pfd3
	pangoLayoutSet pl3 ("Next" :: T.Text)
	fpl3 <- pangoLayoutFreeze pl3
	print =<< pangoLayoutInfo @PixelExtents fpl3

	pure ()

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
