{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.TextAttributes

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

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-attrs.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
