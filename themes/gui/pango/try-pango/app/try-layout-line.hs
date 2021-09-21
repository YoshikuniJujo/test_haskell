{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine
import Graphics.Pango.Basic.VerticalText
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 600 800
	cr <- cairoCreate s
	pl <- pangoCairoCreateLayout cr

	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Size 20
	fd' <- pangoFontDescriptionFreeze fd

	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは、世界!\n\x1f9a5"

	cairoMoveTo cr 50 50
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	pangoLayoutSet pl $ Width 500
	pangoLayoutSet @T.Text pl $
		"親譲りの無鉄砲で小供の時から損ばかりしている。" <>
		"小学校に居る時分学校の二階から疲び降りて" <>
		"一週間ほど腰を抜かした事がある。" <>
		"なぜそんな無闇をしたと聞く人があるかも知れぬ。"

	cairoMoveTo cr 50 200
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	ls <- pangoLayoutGetLines =<< pangoLayoutFreeze pl
	(print <=< pangoLayoutLineGetExtents) `mapM_` ls
	(print <=< pangoLayoutLineGetPixelExtents) `mapM_` ls

	cairoMoveTo cr 50 500
	pangoCairoShowLayoutLine cr $ head ls
	cairoMoveTo cr 50 450
	pangoCairoShowLayoutLine cr $ ls !! 1

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "pngs/try-layout-line.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
