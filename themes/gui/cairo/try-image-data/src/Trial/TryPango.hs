{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryPango where

import Foreign.C.Types
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.CairoContext

import Graphics.Cairo.Drawing.Paths
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations
import Graphics.Pango.Basic.LayoutObjects.PangoLayout as LO
import Graphics.Pango.Basic.TextAttributes
import Graphics.Pango.Rendering.Cairo

import qualified Data.Text as T

import Data.ImageData.Text as I

drawLayout :: CairoTIO s -> Layout -> IO ()
drawLayout cr lyot = do
	pl <- pangoCairoCreateLayout cr
	pangoLayoutSet pl . toWidth . layoutAttrsWidth $ layoutAttrs lyot
	pangoLayoutSet pl . toHeight . layoutAttrsHeight $ layoutAttrs lyot
	pangoLayoutSet pl . toWrap . layoutAttrsWrap $ layoutAttrs lyot
	pangoLayoutSet pl . toEllipsize . layoutAttrsEllipsize $ layoutAttrs lyot
	pangoLayoutSet pl . T.concat . (textText <$>) $ layoutText lyot
	pangoLayoutSet pl $ makeTextAttrList lyot
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

toWidth :: LayoutWidth -> LO.Width
toWidth LayoutWidthDefault = WidthDefault
toWidth (LayoutWidth w) = LO.Width $ realToFrac w

toHeight :: LayoutHeight -> LO.Height
toHeight = \case
	LayoutHeightDefault -> HeightDefault
	LayoutHeight h -> Height $ realToFrac h
	LayoutLines ls -> LinesPerParagraph $ fromIntegral ls

toWrap :: LayoutWrap -> PangoWrapMode
toWrap = \case
	LayoutWrapWord -> PangoWrapWord
	LayoutWrapChar -> PangoWrapChar
	LayoutWrapWordChar -> PangoWrapWordChar

toEllipsize :: LayoutEllipsize -> PangoEllipsizeMode
toEllipsize = \case
	LayoutEllipsizeNone -> PangoEllipsizeNone
	LayoutEllipsizeStart -> PangoEllipsizeStart
	LayoutEllipsizeMiddle -> PangoEllipsizeMiddle
	LayoutEllipsizeEnd -> PangoEllipsizeEnd

makeTextAttrList :: Layout -> PangoTextAttrList
makeTextAttrList lyot = runST do
	pl <- pangoTextAttrListNew . T.concat . (textText <$>) $ layoutText lyot
	forM_ (getAttrs lyot) \(TextAttrs { textAttrsFont = fnt, textAttrsStrikethrough = stthr }, (s, e)) -> do
		fd <- getFont fnt
		attr <- pangoAttrFontDescNew fd
		attrs <- attrStrikethrough stthr
		pangoTextAttrListInsert pl attr s e
		(\a -> pangoTextAttrListInsert pl a s e) `mapM_` attrs
	pangoTextAttrListFreeze pl

attrStrikethrough :: PrimMonad m => I.Strikethrough -> m [PangoAttribute (PrimState m)]
attrStrikethrough NoStrikethrough = pure []
attrStrikethrough StrikethroughWithForegroundColor =
	(: []) <$> pangoAttrNew (Strikethrough True)
attrStrikethrough (StrikethroughWithColor rgb) = do
	a1 <- pangoAttrNew $ Strikethrough True
	a2 <- pangoAttrNew $ StrikethroughColor rgb
	pure [a1, a2]

getAttrs :: Layout -> [(TextAttrs, (Int, Int))]
getAttrs lyot = zip (textAttrs <$> layoutText lyot) (getStartAndEnds lyot)

getStartAndEnds :: Layout -> [(Int, Int)]
getStartAndEnds = startAndEnds . (T.length . textText <$>) . layoutText

startAndEnds :: [Int] -> [(Int, Int)]
startAndEnds [] = []
startAndEnds xs = ps `zip` tail ps
	where ps = scanl (+) 0 xs

getFont :: PrimMonad m => Font -> m (PangoFontDescriptionPrim (PrimState m))
getFont fnt = do
	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd . Family $ getFontFamily fnt
	pangoFontDescriptionSet fd $ getFontSize fnt
	case fnt of
		Font {	fontStyle = s, fontVariant = v, fontWeight = w,
			fontStretch = st } -> do
			pangoFontDescriptionSet fd $ toStyle s
			pangoFontDescriptionSet fd $ toVariant v
			pangoFontDescriptionSet fd $ toWeight w
			pangoFontDescriptionSet fd $ toStretch st
		VariableFont { variableFontVariations = v } ->
			pangoFontDescriptionSetVariationsMap fd v
	pure fd

drawFont :: CairoTIO s -> CDouble -> CDouble -> Font -> T.Text -> IO ()
drawFont cr x y fnt t = do
	pl <- pangoCairoCreateLayout cr

	fd <- pangoFontDescriptionNew
	pangoFontDescriptionSet fd . Family $ getFontFamily fnt
	pangoFontDescriptionSet fd $ getFontSize fnt
	case fnt of
		Font {	fontStyle = s, fontVariant = v, fontWeight = w,
			fontStretch = st } -> do
			pangoFontDescriptionSet fd $ toStyle s
			pangoFontDescriptionSet fd $ toVariant v
			pangoFontDescriptionSet fd $ toWeight w
			pangoFontDescriptionSet fd $ toStretch st
		VariableFont { variableFontVariations = v } ->
			pangoFontDescriptionSetVariationsMap fd v
	fd' <- pangoFontDescriptionFreeze fd

	cairoMoveTo cr x y
	pangoLayoutSet pl . pangoFontDescriptionToNullable $ Just fd'
	pangoLayoutSet @T.Text pl t

	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

getFontFamily :: Font -> String
getFontFamily = \case
	Font { fontFamily = ff } -> ff
	VariableFont { variableFontFamily = ff } -> ff

getFontSize :: Font -> Size
getFontSize = \case
	Font { fontSize = s } -> toSize s
	VariableFont { variableFontSize = s } -> toSize s

toSize :: FontSize -> Size
toSize (FontSize s) = Size $ realToFrac s
toSize (AbsoluteFontSize s) = AbsoluteSize $ realToFrac s

toStyle :: FontStyle -> PangoStyle
toStyle = \case
	StyleNormal -> PangoStyleNormal
	StyleOblique -> PangoStyleOblique
	StyleItalic -> PangoStyleItalic

toVariant :: FontVariant -> PangoVariant
toVariant = \case
	VariantNormal -> PangoVariantNormal
	VariantSmallCaps -> PangoVariantSmallCaps

toWeight :: FontWeight -> PangoWeight
toWeight = \case
	WeightThin -> PangoWeightThin
	WeightUltralight -> PangoWeightUltralight
	WeightLight -> PangoWeightLight
	WeightSemilight -> PangoWeightSemilight
	WeightBook -> PangoWeightBook
	WeightNormal -> PangoWeightNormal
	WeightMedium -> PangoWeightMedium
	WeightSemibold -> PangoWeightSemibold
	WeightBold -> PangoWeightBold
	WeightUltrabold -> PangoWeightUltrabold
	WeightHeavy -> PangoWeightHeavy
	WeightUltraheavy -> PangoWeightUltraheavy

toStretch :: FontStretch -> PangoStretch
toStretch = \case
	StretchUltraCondensed -> PangoStretchUltraCondensed
	StretchExtraCondensed -> PangoStretchExtraCondensed
	StretchCondensed -> PangoStretchCondensed
	StretchSemiCondensed -> PangoStretchSemiCondensed
	StretchNormal -> PangoStretchNormal
	StretchSemiExpanded -> PangoStretchSemiExpanded
	StretchExpanded -> PangoStretchExpanded
	StretchExtraExpanded -> PangoStretchExtraExpanded
	StretchUltraExpanded -> PangoStretchUltraExpanded
