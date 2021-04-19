{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Primitive
import Data.Char
import Data.CairoImage
import Data.JuicyCairo
import System.Environment
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations hiding (Weight(..))
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import qualified Data.Text as T

pangoFontDescriptionAddAxis "Inline" "BLDA"
pangoFontDescriptionAddAxis "Worm" "BLDB"
pangoFontDescriptionAddAxis "Weight" "WMX2"
pangoFontDescriptionAddAxis "InlineSkeleton" "SKLA"
pangoFontDescriptionAddAxis "WormSkeleton" "SKLB"
pangoFontDescriptionAddAxis "Stripes" "SKLD"
pangoFontDescriptionAddAxis "Rounded" "TRMA"
pangoFontDescriptionAddAxis "Flared" "TRMB"
pangoFontDescriptionAddAxis "RoundedSlab" "TRMC"
pangoFontDescriptionAddAxis "Sheared" "TRMD"
pangoFontDescriptionAddAxis "Bifurcated" "TRME"
pangoFontDescriptionAddAxis "OpenInlineTerminal" "TRMF"
pangoFontDescriptionAddAxis "Slab" "TRMG"
pangoFontDescriptionAddAxis "InlineTerminal" "TRMK"
pangoFontDescriptionAddAxis "WormTerminal" "TRML"

main :: IO ()
main = getArgs >>= \case
	blda : bldb : wmx2 : skla : sklb : skld :
			trma : trmb : trmc : trmd : trme : trmf : trmg : trmk : trml : _ -> do
		s <- cairoImageSurfaceCreate cairoFormatArgb32 1000 400
		cr <- cairoCreate s

		fd <- pangoFontDescriptionNew

		pangoFontDescriptionSetFamily fd "Decovar Alpha"
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
		putStrLn $ pangoFontDescriptionToString fd'
		putStrLn $ pangoFontDescriptionToFilename fd'

		pl <- pangoCairoCreateLayout cr
		pangoLayoutSetFontDescription pl fd'
		pangoLayoutSet @T.Text pl "Hello, world!\nこんにちは世界!"
		pangoCairoShowLayout cr pl

		cairoImageSurfaceGetCairoImage s >>= \case
			CairoImageArgb32 a -> writePng "try-pango-fonts-variations.png" $ cairoArgb32ToJuicyRGBA8 a
			_ -> error "never occur"
	_ -> error "need blda and skla"

setAxisFromString :: (PangoFontDescriptionAxis a, PrimMonad m) =>
	PangoFontDescriptionPrim (PrimState m) -> (Double -> a) -> String -> m ()
setAxisFromString fd mk = \case
	a | all ((||) <$> isDigit <*> (== '.')) a -> pangoFontDescriptionSetAxis fd . mk $ read a
	_ -> pure ()
