{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData.Text where

import Data.Maybe
import Data.Color
import Data.Font.VariationAxis

import qualified Data.Text as T

import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage

data Layout = Layout {
	layoutAttrs :: LayoutAttrs,
	layoutText :: [Text] }
	deriving Show

data LayoutAttrs = LayoutAttrs {
	layoutAttrsWidth :: LayoutWidth,
	layoutAttrsHeight :: LayoutHeight,
	layoutAttrsWrap :: LayoutWrap,
	layoutAttrsEllipsize :: LayoutEllipsize,
	layoutAttrsIndent :: LayoutIndent,
	layoutAttrsLineSpacing :: LayoutLineSpacing,
	layoutAttrsJustify :: LayoutJustify }
	deriving Show

data LayoutWidth = LayoutWidthDefault | LayoutWidth Double deriving Show

data LayoutHeight = LayoutHeightDefault | LayoutHeight Double | LayoutLines Int
	deriving Show

data LayoutWrap = LayoutWrapWord | LayoutWrapChar | LayoutWrapWordChar
	deriving Show

data LayoutEllipsize
	= LayoutEllipsizeNone
	| LayoutEllipsizeStart | LayoutEllipsizeMiddle | LayoutEllipsizeEnd
	deriving Show

newtype LayoutIndent = LayoutIndent Double deriving Show

newtype LayoutLineSpacing = LayoutLineSpacing Double deriving Show

newtype LayoutJustify = LayoutJustify Bool deriving Show

sampleForAutoDir :: Layout
sampleForAutoDir = Layout {
	layoutAttrs = defaultLayoutAttrs,
	layoutText = [arabic] }

arabic :: Text
arabic = Text (textAttrsFromFont $ sampleFont "sans" 16)
	$ pangoLanguageGetSampleString $ PangoLanguage "ar"

sampleForJustify :: LayoutJustify -> Layout
sampleForJustify j = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 600,
		layoutAttrsJustify = j },
	layoutText = [genesis] }

sampleForLineSpacing :: LayoutLineSpacing -> Layout
sampleForLineSpacing s = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 600,
		layoutAttrsLineSpacing = s },
	layoutText = [genesis] }

sampleForHeight :: LayoutWidth -> LayoutEllipsize -> LayoutHeight -> Layout
sampleForHeight wdt elp = setSampleHeight $ sampleForWrap wdt LayoutWrapWord elp

setSampleHeight :: Layout -> LayoutHeight -> Layout
setSampleHeight (Layout a t) h = Layout a { layoutAttrsHeight = h } t

sampleForIndent :: LayoutIndent -> Layout
sampleForIndent i = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 600, layoutAttrsIndent = i },
	layoutText = [genesis] }

defaultLayoutAttrs :: LayoutAttrs
defaultLayoutAttrs = LayoutAttrs {
	layoutAttrsWidth = LayoutWidthDefault,
	layoutAttrsHeight = LayoutHeightDefault,
	layoutAttrsWrap = LayoutWrapWord,
	layoutAttrsEllipsize = LayoutEllipsizeNone,
	layoutAttrsIndent = LayoutIndent 0,
	layoutAttrsLineSpacing = LayoutLineSpacing 0,
	layoutAttrsJustify = LayoutJustify False }

genesis :: Text
genesis = Text (textAttrsFromFont $ sampleFont "sans" 16) $
		"In the beginning God created the heaven and the earth.\n" <>
		"And the earth was without form, and void; " <>
		"and darkness was upon the face of the deep. " <>
		"And the Spirit of God moved upon the face of the waters. " <>
		"And God said, Let there be light: and there was light."

sampleForWrap :: LayoutWidth -> LayoutWrap -> LayoutEllipsize -> Layout
sampleForWrap wdt wrp elp = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = wdt, layoutAttrsWrap = wrp,
		layoutAttrsEllipsize = elp },
	layoutText = [genesis] }

sampleForWidth :: LayoutWidth -> Layout
sampleForWidth w = Layout {
	layoutAttrs = defaultLayoutAttrs { layoutAttrsWidth = w },
	layoutText = [
		Text (textAttrsFromFont $ sampleFont "sazanami" 32) $
			"いろはにほへとちりぬるをわかよたれそつねならぬ" <>
			"うゐのおくやまけふこえてあさきゆめみしゑひもせす"
		]
	}

sampleLayout :: Layout
sampleLayout = Layout {
	layoutAttrs = defaultLayoutAttrs,
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
