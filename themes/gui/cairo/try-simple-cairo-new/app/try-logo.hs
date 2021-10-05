{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad
import Data.Maybe
import Data.Bool
import Data.Color
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Drawing.CairoT.CairoOperatorT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Rendering.Cairo

import MakePng

import qualified Data.Text as T

rotate :: CairoTIO s -> CDouble -> IO ()
rotate cr a = do
	cairoTranslate cr 128 128
	cairoRotate cr a
	cairoTranslate cr (- 128) (- 128)

put :: CairoTIO s -> PangoFontDescriptionNullable -> CDouble -> T.Text -> IO PangoFixed
put cr fd a t = do
	rotate cr a
	cairoMoveTo cr 116 16
	pl <- pangoCairoCreateLayout cr
	pangoLayoutSet pl fd
	pangoLayoutSet pl t
	pl' <- pangoLayoutFreeze pl
	print . pangoRectangleFixedWidth . extentsLogicalRect =<< pangoLayoutInfo pl'
	pangoCairoShowLayout cr pl'
	cairoIdentityMatrix cr
	w <- pangoRectangleFixedWidth
		. extentsLogicalRect <$> pangoLayoutInfo pl'
	pure $ bool w (w * 1 / 2) $ t == " "

layoutArc :: CairoTIO s -> PangoFontDescriptionNullable -> CDouble -> String -> IO ()
layoutArc cr fd a0 cs = (\f -> foldM_ f a0 cs) \a c -> do
	w <- put cr fd a $ T.singleton c
	pure $ a + (pi / 16) * (realToFrac w / 16)
{-
layoutArc cr fd a0 cs = for_ ([a0, a0 + (pi / 16) ..] `zip` cs) \(a, c) ->
	put cr fd a $ T.singleton c
	-}

main :: IO ()
main = pngWith "pngs/try-logo.png" 256 256 \cr -> do
--	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.3 0.3 0.3
--	cairoPaint cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.15 0.15 0.15
	cairoArc cr 128 128 128 0 (2 * pi)
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.8 0.8
	cairoArc cr 128 128 80 0 (2 * pi)
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.15 0.15 0.15
	cairoSet cr OperatorClear
	cairoSet cr $ LineWidth 12
	cairoSet cr LineCapRound
	cairoSet cr $ Dash [32, 20] 0
	cairoArc cr 128 128 68 (- pi / 2) (3 / 2 * pi)
	cairoStroke cr

	cairoSet cr OperatorOver

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.8 0.8
	fd_ <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd_ $ Family "sans"
	pangoFontDescriptionSet fd_ PangoWeightBold
	pangoFontDescriptionSet fd_ $ Size 16
	fd <- pangoFontDescriptionToNullable . Just
		<$> pangoFontDescriptionFreeze fd_
	layoutArc cr fd 0 "WAKAYAMA UNIVERSITY"

	layoutArc cr fd (5 / 4 * pi) "RACINGTEAM"

{-
	rotate cr $ pi / 2

	pl <- pangoCairoCreateLayout cr
	pangoLayoutSet pl . pangoFontDescriptionToNullable . Just
		=<< pangoFontDescriptionFreeze fd
	pangoLayoutSet @T.Text pl "\x01f9a5ナマケモノ"

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 0.05
	cairoMoveTo cr 24 32
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.0 0.7 0.0
	cairoMoveTo cr 24 96
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.6 0.4 0.2
	cairoMoveTo cr 24 160
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
	-}
