{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryPango where

import Foreign.C.Types
import Control.Arrow
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Color
import Data.CairoContext

import Graphics.Cairo.Drawing.Paths
import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations
import Graphics.Pango.Basic.LayoutObjects.PangoLayout as LO
import Graphics.Pango.Basic.TextAttributes as P
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
	pangoLayoutSet pl . toIndent . layoutAttrsIndent $ layoutAttrs lyot
	pangoLayoutSet pl . toLineSpacing . layoutAttrsLineSpacing $ layoutAttrs lyot
	pangoLayoutSet pl . toJustify . layoutAttrsJustify $ layoutAttrs lyot
	pangoLayoutSet pl . toAutoDir . layoutAttrsAutoDir $ layoutAttrs lyot
	pangoLayoutSet pl . toAlignment . layoutAttrsAlignment $ layoutAttrs lyot
	pangoLayoutSet pl . toSingleParagraph . layoutAttrsSingleParagraph $ layoutAttrs lyot
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

toIndent :: LayoutIndent -> Indent
toIndent (LayoutIndent i) = Indent $ realToFrac i

toLineSpacing :: LayoutLineSpacing -> LineSpacing
toLineSpacing (LayoutLineSpacing s) = LineSpacing $ realToFrac s

toJustify :: LayoutJustify -> Justify
toJustify (LayoutJustify j) = Justify j

toAutoDir :: LayoutAutoDir -> AutoDir
toAutoDir (LayoutAutoDir ad) = AutoDir ad

toAlignment :: LayoutAlignment -> PangoAlignment
toAlignment = \case
	LayoutAlignLeft -> PangoAlignLeft
	LayoutAlignCenter -> PangoAlignCenter
	LayoutAlignRight -> PangoAlignRight

toSingleParagraph :: LayoutSingleParagraph -> SingleParagraphMode
toSingleParagraph (LayoutSingleParagraph sp) = SingleParagraphMode sp

makeTextAttrList :: Layout -> PangoTextAttrList
makeTextAttrList lyot = runST do
	pl <- pangoTextAttrListNew . T.concat . (textText <$>) $ layoutText lyot
	forM_ (getAttrs lyot) \(TextAttrs {
		textAttrsFont = fnt,
		textAttrsStrikethrough = stthr,
		textAttrsUnderline = ul,
		textAttrsShape = msp,
		textAttrsScale = scl,
		textAttrsRise = rs,
		textAttrsLetterSpacing = ls,
		textAttrsForegroundColor = fc,
		textAttrsBackgroundColor = bc }, (s, e)) -> do
		fd <- getFont fnt
		attr <- pangoAttrFontDescNew fd
		attrs <- attrStrikethrough stthr
		attrs2 <- attrUnderline ul
		attr2 <- maybe (pure []) (((: []) <$>) . attrShape) msp
		attr3 <- (((: []) <$>) . attrScale) scl
		attr4 <- (: []) <$> attrRise rs
		attr5 <- (: []) <$> attrLetterSpacing ls
		attrs3 <- maybe (pure []) attrForegroundColor fc
		attrs4 <- maybe (pure []) attrBackgroundColor bc
		pangoTextAttrListInsert pl attr s e
		(\a -> pangoTextAttrListInsert pl a s e) `mapM_` (
			attrs ++ attrs2 ++ attr2 ++ attr3 ++ attr4 ++ attr5 ++ attrs3 ++ attrs4)
	pangoTextAttrListFreeze pl

attrStrikethrough :: PrimMonad m => I.Strikethrough -> m [PangoAttribute (PrimState m)]
attrStrikethrough StrikethroughNone = pure []
attrStrikethrough StrikethroughWithForegroundColor =
	(: []) <$> pangoAttrNew (Strikethrough True)
attrStrikethrough (StrikethroughWithColor rgb) = do
	a1 <- pangoAttrNew $ Strikethrough True
	a2 <- pangoAttrNew $ StrikethroughColor rgb
	pure [a1, a2]

attrUnderline :: PrimMonad m => Underline -> m [PangoAttribute (PrimState m)]
attrUnderline ul = (:)
	<$> pangoAttrNew pul
	<*> maybe (pure []) (((: []) <$>) . pangoAttrNew . UnderlineColor) mc
	where (pul, mc) = toUnderline ul

toUnderline :: Underline -> (PangoUnderline, Maybe (Rgb Double))
toUnderline = \case
	UnderlineNone -> (PangoUnderlineNone, Nothing)
	Underline us mc -> (toUnderlineStyle us, mc)

toUnderlineStyle :: UnderlineStyle -> PangoUnderline
toUnderlineStyle = \case
	UnderlineSingle -> PangoUnderlineSingle
	UnderlineDouble -> PangoUnderlineDouble
	UnderlineLow -> PangoUnderlineLow
	UnderlineError -> PangoUnderlineError

attrShape :: PrimMonad m => I.Shape -> m (PangoAttribute (PrimState m))
attrShape = pangoAttrNew . toShape

toShape :: I.Shape -> P.Shape
toShape (I.Shape ir lr) = P.Shape (toRectangle ir) (toRectangle lr)

toRectangle :: Rectangle -> PangoRectangleFixed
toRectangle (Rectangle x y w h) =
	PangoRectangleFixed (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h)

attrScale :: PrimMonad m => I.Scale -> m (PangoAttribute (PrimState m))
attrScale = pangoAttrNew . toScale

toScale :: I.Scale -> P.Scale
toScale (I.Scale scl) = P.Scale $ realToFrac scl

attrRise :: PrimMonad m => I.Rise -> m (PangoAttribute (PrimState m))
attrRise = pangoAttrNew . toRise

toRise :: I.Rise -> P.Rise
toRise (I.Rise rs) = P.Rise $ realToFrac rs

attrLetterSpacing :: PrimMonad m => I.LetterSpacing -> m (PangoAttribute (PrimState m))
attrLetterSpacing = pangoAttrNew . toLetterSpacing

toLetterSpacing :: I.LetterSpacing -> P.LetterSpacing
toLetterSpacing (I.LetterSpacing ls) = P.LetterSpacing $ realToFrac ls

attrForegroundColor :: PrimMonad m => I.ForegroundColor -> m [PangoAttribute (PrimState m)]
attrForegroundColor (toForegroundColor -> (c, a)) = (\a1 a2 -> [a1, a2])
	<$> pangoAttrNew c <*> pangoAttrNew a

toForegroundColor :: I.ForegroundColor -> (P.ForegroundColor Double, P.ForegroundAlpha Double)
toForegroundColor (I.ForegroundColor rgba) =
	(P.ForegroundColor *** P.ForegroundAlpha) $ fromRgba rgba

attrBackgroundColor :: PrimMonad m => I.BackgroundColor -> m [PangoAttribute (PrimState m)]
attrBackgroundColor (toBackgroundColor -> (c, a)) = (\a1 a2 -> [a1, a2])
	<$> pangoAttrNew c <*> pangoAttrNew a

toBackgroundColor :: I.BackgroundColor -> (P.BackgroundColor Double, P.BackgroundAlpha Double)
toBackgroundColor (I.BackgroundColor rgba) =
	(P.BackgroundColor *** P.BackgroundAlpha) $ fromRgba rgba

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
