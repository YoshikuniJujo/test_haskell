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
	layoutAttrsJustify :: LayoutJustify,
	layoutAttrsAutoDir :: LayoutAutoDir,
	layoutAttrsAlignment :: LayoutAlignment,
	layoutAttrsSingleParagraph :: LayoutSingleParagraph }
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

newtype LayoutAutoDir = LayoutAutoDir Bool deriving Show

data LayoutAlignment
	= LayoutAlignLeft | LayoutAlignCenter | LayoutAlignRight deriving Show

newtype LayoutSingleParagraph = LayoutSingleParagraph Bool deriving Show

sampleTeX :: Layout
sampleTeX = Layout {
	layoutAttrs = defaultLayoutAttrs,
	layoutText = [
		Text (textAttrsFromFont $ sampleFont "sans" 32) "T",
		Text (textAttrsFromFont $ sampleFont "sans" 32)
			{ textAttrsRise = Rise $ - 16 } "E",
		Text (textAttrsFromFont $ sampleFont "sans" 32) "X" ] }

sampleForRise :: Layout
sampleForRise = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 600 },
	layoutText =
		zipWith risedText
			(Rise <$> [0, 0.5 .. ])
			(T.singleton <$> ['a' .. 'z']) ++
		zipWith risedText
			(Rise <$> [0, - 0.5 .. ])
			(T.singleton <$> ['あ' .. 'ん']) }

risedText :: Rise -> T.Text -> Text
risedText rs = Text (textAttrsFromFont $ sampleFont "sans" 16)
	{ textAttrsRise = rs }

sampleForScale :: Layout
sampleForScale = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 600 },
	layoutText =
		zipWith scaledText
			(Scale <$> [0.1, 0.2 ..])
			(T.singleton <$> ['a' .. 'z']) ++
		zipWith scaledText
			(Scale <$> [0.1, 0.13 ..])
			(T.singleton <$> ['あ' .. 'ん']) }

scaledText :: Scale -> T.Text -> Text
scaledText scl = Text (textAttrsFromFont $ sampleFont "sans" 16)
	{ textAttrsScale = scl }

sampleForShape :: Layout
sampleForShape = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 300 },
	layoutText = [
		Text (textAttrsFromFont $ sampleFont "sans" 32) "abc",
		Text (textAttrsFromFont $ sampleFont "sans" 32)
			{ textAttrsShape = Just $ Shape (Rectangle 5 10 50 50) (Rectangle 0 0 70 70) } "def",
		Text (textAttrsFromFont $ sampleFont "sans" 32) "ghi\njklmnopqrstu" ] }

sampleForUnderline :: [Underline] -> Layout
sampleForUnderline uls = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 600,
		layoutAttrsLineSpacing = LayoutLineSpacing 1.5 },
	layoutText = underlineToText <$> uls }

underlineToText :: Underline -> Text
underlineToText ul =
	Text (textAttrsFromFont $ sampleFont "sans" 32)
		{ textAttrsUnderline = ul } "あいうえお Hello, world!\n"

sampleForSingleParagraph :: LayoutSingleParagraph -> Layout
sampleForSingleParagraph sp = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 600,
		layoutAttrsSingleParagraph = sp },
	layoutText = [genesis] }

sampleForAlignment :: LayoutAlignment -> Layout
sampleForAlignment aln = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = LayoutWidth 600,
		layoutAttrsAlignment = aln },
	layoutText = [genesis] }

sampleForAutoDir :: LayoutWidth -> LayoutAutoDir -> Layout
sampleForAutoDir w ad = Layout {
	layoutAttrs = defaultLayoutAttrs {
		layoutAttrsWidth = w, layoutAttrsAutoDir = ad },
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
	layoutAttrsJustify = LayoutJustify False,
	layoutAttrsAutoDir = LayoutAutoDir True,
	layoutAttrsAlignment = LayoutAlignLeft,
	layoutAttrsSingleParagraph = LayoutSingleParagraph False }

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
	textAttrsStrikethrough = StrikethroughNone,
	textAttrsUnderline = UnderlineNone,
	textAttrsShape = Nothing,
	textAttrsScale = Scale 1,
	textAttrsRise = Rise 0 }

data TextAttrs = TextAttrs {
	textAttrsFont :: Font,
	textAttrsStrikethrough :: Strikethrough,
	textAttrsUnderline :: Underline,
	textAttrsShape :: Maybe Shape,
	textAttrsScale :: Scale,
	textAttrsRise :: Rise }
	deriving Show

data Strikethrough
	= StrikethroughNone
	| StrikethroughWithForegroundColor
	| StrikethroughWithColor (Rgb Double)
	deriving Show

data Underline
	= UnderlineNone
	| Underline UnderlineStyle (Maybe (Rgb Double))
	deriving Show

data UnderlineStyle
	= UnderlineSingle | UnderlineDouble | UnderlineLow | UnderlineError
	deriving Show

data Shape = Shape Rectangle Rectangle deriving Show

data Rectangle = Rectangle {
	rectangleX, rectangleY :: Double,
	rectangleWidth, rectanbleHeight :: Double }
	deriving Show

newtype Scale = Scale Double deriving Show

newtype Rise = Rise Double deriving Show

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
