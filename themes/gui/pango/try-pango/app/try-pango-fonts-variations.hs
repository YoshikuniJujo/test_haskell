{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Primitive
import Data.Char
import Data.CairoImage
import Data.JuicyCairo
import Data.Font.VariationAxis hiding (Weight(..))
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations hiding (Weight(..))

import qualified Data.Text as T

fontDescriptionAddAxis "Inline" "BLDA"
fontDescriptionAddAxis "Worm" "BLDB"
fontDescriptionAddAxis "Weight" "WMX2"
fontDescriptionAddAxis "InlineSkeleton" "SKLA"
fontDescriptionAddAxis "WormSkeleton" "SKLB"
fontDescriptionAddAxis "Stripes" "SKLD"
fontDescriptionAddAxis "Rounded" "TRMA"
fontDescriptionAddAxis "Flared" "TRMB"
fontDescriptionAddAxis "RoundedSlab" "TRMC"
fontDescriptionAddAxis "Sheared" "TRMD"
fontDescriptionAddAxis "Bifurcated" "TRME"
fontDescriptionAddAxis "OpenInlineTerminal" "TRMF"
fontDescriptionAddAxis "Slab" "TRMG"
fontDescriptionAddAxis "InlineTerminal" "TRMK"
fontDescriptionAddAxis "WormTerminal" "TRML"

main :: IO ()
main = getArgs >>= \case
	blda : bldb : wmx2 : skla : sklb : skld :
			trma : trmb : trmc : trmd : trme : trmf : trmg : trmk : trml : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 1000 400
		cr <- cairoCreate s

		fd <- pangoFontDescriptionNew

		pangoFontDescriptionSet fd $ Family "Decovar Alpha"
		pangoFontDescriptionSet fd $ Size 80

		setAxisFromString fd Inline blda
		setAxisFromString fd Worm bldb
		setAxisFromString fd Weight wmx2
		setAxisFromString fd InlineSkeleton skla
		setAxisFromString fd WormSkeleton sklb
		setAxisFromString fd Stripes skld
		setAxisFromString fd Rounded trma
		setAxisFromString fd Flared trmb
		setAxisFromString fd RoundedSlab trmc
		setAxisFromString fd Sheared trmd
		setAxisFromString fd Bifurcated trme
		setAxisFromString fd OpenInlineTerminal trmf
		setAxisFromString fd Slab trmg
		setAxisFromString fd InlineTerminal trmk
		setAxisFromString fd WormTerminal trml

		fd' <- pangoFontDescriptionFreeze fd
		print $ pangoFontDescriptionToString fd'
		print $ pangoFontDescriptionToFilename fd'

		pl <- pangoCairoCreateLayout cr
		pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
		pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは世界!"
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-variations.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "need blda and skla"

setAxisFromString :: (FontDescriptionAxis a, PrimMonad m) =>
	PangoFontDescriptionPrim (PrimState m) -> (Double -> a) -> String -> m ()
setAxisFromString fd mk = \case
	a | all ((||) <$> isDigit <*> (== '.')) a -> pangoFontDescriptionSetAxis fd . mk $ read a
	_ -> pure ()
