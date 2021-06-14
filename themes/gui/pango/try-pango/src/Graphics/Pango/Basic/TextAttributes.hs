{-# LANGUAGE PatternSynonyms #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TextAttributes (
	-- * TYPE
	PangoTextAttrList, PangoTextAttrListPrim,
	pangoTextAttrListNew,
	pangoTextAttrListFreeze, pangoTextAttrListThaw,
	pangoTextAttrListCopy,

	-- * PARSE MARKUP
	pangoParseMarkup, pangoMarkupParserNew, pangoMarkupParserFinish,

	-- * INSERT AN ATTRIBUTE TO PANGO TEXT ATTRIBUTE LIST FOR PRIMITIVE MONAD
	pangoTextAttrListInsert, pangoTextAttrListInsertBefore,
	pangoTextAttrListChange,

	-- * PANGO ATTRIBUTE VALUE
	-- ** Class
	PangoAttributeValue, PangoAttribute, pangoAttrNew,

	-- ** Instance
	-- *** FontDescription
	PangoFontDescriptionPrim,
	pangoAttrFontDescNew,

	-- *** Strikethrough and StrikethroughColor
	Strikethrough(..),
	StrikethroughColor(..),

	-- *** PangoUnderline and UnderlineColor
	PangoUnderline, pattern PangoUnderlineNone,
	pattern PangoUnderlineSingle, pattern PangoUnderlineDouble,
	pattern PangoUnderlineLow, pattern PangoUnderlineError,

	UnderlineColor(..),

	-- *** Shape
	Shape(..),

	-- *** Scale
	Scale(..),

	-- *** Rise
	Rise, pattern Rise,

	-- *** LetterSpacing
	LetterSpacing, pattern LetterSpacing,

	-- *** Color and Alpha of Foreground and Background
	ForegroundColor(..), BackgroundColor(..),
	ForegroundAlpha(..), BackgroundAlpha(..),

	-- * PANGO COLOR
	PangoColor(..), pangoColorParse, pangoColorToString,
	) where

import Graphics.Pango.Basic.TextAttributes.Internal
import Graphics.Pango.Basic.Fonts.PangoFontDescription
