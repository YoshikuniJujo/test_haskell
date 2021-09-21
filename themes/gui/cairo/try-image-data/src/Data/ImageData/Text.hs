{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData.Text where

import Data.Font.VariationAxis

import qualified Data.Text as T

data Layout = Layout {
	layoutText :: [Text] }
	deriving Show

sampleLayout :: Layout
sampleLayout = Layout [
	Text (sampleFont "soulcraft" 32) "abc",
	Text (sampleFont "sazanami" 32) "あいう",
	Text (sampleFont "sans" 32) "def",
	Text (sampleFont "serif" 32) "ghi\n",
	Text (sourceHanSansVf 32 100) "あいう",
	Text (sourceHanSansVf 32 300) "えお"]

sampleFont :: String -> Double -> Font
sampleFont fm sz = Font {
	fontFamily = fm,
	fontSize = FontSize sz,
	fontStyle = StyleNormal,
	fontVariant = VariantNormal,
	fontWeight = WeightNormal,
	fontStretch = StretchNormal }

sourceHanSansVf :: Double -> Double -> Font
sourceHanSansVf sz wgt =
	VariableFont "Source Han Sans VF" (FontSize sz) (weight wgt)

weight :: Double -> Variations
weight w = variationsSetAxis (Weight w) variationsEmpty

data Text = Text { textFont :: Font, textText :: T.Text } deriving Show

data Font
	= Font {
		fontFamily :: String,
		fontSize :: FontSize,
		fontStyle :: FontStyle,
		fontVariant :: FontVariant,
		fontWeight :: FontWeight,
		fontStretch :: FontStretch }
	| VariableFont {
		variableFontFamily :: String,
		variableFontSize :: FontSize,
		variableFontVariations :: Variations }
	deriving Show

data FontSize = FontSize Double | AbsoluteFontSize Double deriving Show

data FontStyle = StyleNormal | StyleOblique | StyleItalic deriving Show

data FontVariant = VariantNormal | VariantSmallCaps deriving Show

data FontWeight
	= WeightThin | WeightUltralight | WeightLight | WeightSemilight
	| WeightBook | WeightNormal | WeightMedium | WeightSemibold
	| WeightBold | WeightUltrabold | WeightHeavy | WeightUltraheavy
	deriving (Show, Enum)

data FontStretch
	= StretchUltraCondensed | StretchExtraCondensed | StretchCondensed
	| StretchSemiCondensed | StretchNormal | StretchSemiExpanded
	| StretchExpanded | StretchExtraExpanded | StretchUltraExpanded
	deriving Show
