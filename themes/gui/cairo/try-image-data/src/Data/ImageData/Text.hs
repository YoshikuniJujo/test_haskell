{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData.Text where

import Data.Maybe
import Data.Color
import Data.Font.VariationAxis

import qualified Data.Text as T

data Layout = Layout {
	layoutAttrs :: LayoutAttrs,
	layoutText :: [Text] }
	deriving Show

data LayoutAttrs = LayoutAttrs {
	layoutAttrsWidth :: LayoutWidth,
	layoutAttrsWrap :: LayoutWrap,
	layoutAttrsEllipsize :: LayoutEllipsize }
	deriving Show

data LayoutWidth = LayoutWidthDefault | LayoutWidth Double deriving Show

data LayoutWrap = LayoutWrapWord | LayoutWrapChar | LayoutWrapWordChar
	deriving Show

data LayoutEllipsize
	= LayoutEllipsizeNone
	| LayoutEllipsizeStart | LayoutEllipsizeMiddle | LayoutEllipsizeEnd
	deriving Show

sampleForWrap :: LayoutWidth -> LayoutWrap -> LayoutEllipsize -> Layout
sampleForWrap wdt wrp elp = Layout {
	layoutAttrs = LayoutAttrs {
		layoutAttrsWidth = wdt,
		layoutAttrsWrap = wrp,
		layoutAttrsEllipsize = elp },
	layoutText = [
		Text (textAttrsFromFont $ sampleFont "sans" 16) $
			"In the beginning God created the heaven and the earth.\n" <>
			"And the earth was without form, and void; " <>
			"and darkness was upon the face of the deep. " <>
			"And the Spirit of God moved upon the face of the waters. " <>
			"And God said, Let there be light: and there was light."
		]
	}

sampleForWidth :: LayoutWidth -> Layout
sampleForWidth w = Layout {
	layoutAttrs = LayoutAttrs {
		layoutAttrsWidth = w,
		layoutAttrsWrap = LayoutWrapWord,
		layoutAttrsEllipsize = LayoutEllipsizeNone
		},
	layoutText = [
		Text (textAttrsFromFont $ sampleFont "sazanami" 32) $
			"いろはにほへとちりぬるをわかよたれそつねならぬ" <>
			"うゐのおくやまけふこえてあさきゆめみしゑひもせす"
		]
	}

sampleLayout :: Layout
sampleLayout = Layout {
	layoutAttrs = LayoutAttrs {
		layoutAttrsWidth = LayoutWidthDefault,
		layoutAttrsWrap = LayoutWrapWord,
		layoutAttrsEllipsize = LayoutEllipsizeNone
		},
	layoutText = [
		Text (textAttrsFromFont $ sampleFont "soulcraft" 32) "abc",
		Text (textAttrsFromFont $ sampleFont "sazanami" 32) {
			textAttrsStrikethrough = StrikethroughWithForegroundColor } "あいう",
		Text (textAttrsFromFont $ sampleFont "sans" 32) "def",
		Text (textAttrsFromFont $ sampleFont "serif" 32) {
			textAttrsStrikethrough = StrikethroughWithColor . fromJust $ rgbDouble 0.2 0.5 0.1 } "ghi\n",
		Text (textAttrsFromFont $ sourceHanSansVf 32 100) "あいう",
		Text (textAttrsFromFont $ sourceHanSansVf 32 300) "えお"]
	}

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

data Text = Text { textAttrs :: TextAttrs, textText :: T.Text } deriving Show

textAttrsFromFont :: Font -> TextAttrs
textAttrsFromFont fnt = TextAttrs {
	textAttrsFont = fnt,
	textAttrsStrikethrough = NoStrikethrough }

data TextAttrs = TextAttrs {
	textAttrsFont :: Font,
	textAttrsStrikethrough :: Strikethrough }
	deriving Show

data Strikethrough
	= NoStrikethrough
	| StrikethroughWithForegroundColor
	| StrikethroughWithColor (Rgb Double)
	deriving Show

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
