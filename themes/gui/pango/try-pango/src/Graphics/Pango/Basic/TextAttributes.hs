{-# LANGUAGE PatternSynonyms #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TextAttributes (
	-- * PangoTextAttrList and PangoTextAttrListPrim
	PangoTextAttrList, PangoTextAttrListPrim,
	pangoTextAttrListNew,
	pangoTextAttrListFreeze, pangoTextAttrListThaw,
	pangoTextAttrListCopy,

	-- * Parse Markup
	pangoParseMarkup, pangoMarkupParserNew, pangoMarkupParserFinish,

	-- * Insert an Attribute to PangoTextAttrListPrim
	pangoTextAttrListInsert, pangoTextAttrListInsertBefore,
	pangoTextAttrListChange,

	-- * PangoAttributeValue
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

	-- *** PangoGravity and PangoGravityHint
	PangoGravity(..),
	pattern PangoGravitySouth, pattern PangoGravityEast,
	pattern PangoGravityNorth, pattern PangoGravityWest,
	pattern PangoGravityAuto,

	PangoGravityHint(..),
	pattern PangoGravityHintNatural, pattern PangoGravityHintStrong,
	pattern PangoGravityHintLine,

	-- * PangoColor
	PangoColor(..), pangoColorParse, pangoColorToString,
	) where

import Graphics.Pango.Basic.TextAttributes.Internal
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.VerticalText
