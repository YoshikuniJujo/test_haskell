{-# LANGUAGE PatternSynonyms #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TextAttributes (
	-- * PangoTextAttrList and PangoTextAttrListPrim
	PangoTextAttrList, PangoTextAttrListPrim,
	pangoTextAttrListNew, pangoTextAttrListCopy,
	pangoTextAttrListFreeze, pangoTextAttrListThaw,

	-- * Parse Markup
	pangoParseMarkup, pangoMarkupParserNew, pangoMarkupParserFinish,

	-- * Insert an Attribute to PangoTextAttrListPrim
	pangoTextAttrListInsert, pangoTextAttrListInsertBefore,
	pangoTextAttrListChange,

	-- * PangoAttributeValue
	-- ** Class
	PangoAttributeValue, pangoAttrNew,

	-- ** Instance
	pangoAttrFontDescNew,
	Strikethrough(..), StrikethroughColor(..),
	PangoUnderline, pattern PangoUnderlineNone,
	pattern PangoUnderlineSingle, pattern PangoUnderlineDouble,
	pattern PangoUnderlineLow, pattern PangoUnderlineError,
	UnderlineColor(..), Shape(..), Scale(..),
	Rise, pattern Rise, LetterSpacing, pattern LetterSpacing,
	ForegroundColor(..), BackgroundColor(..),
	ForegroundAlpha(..), BackgroundAlpha(..),
	PangoGravity(..), PangoGravityHint(..),

	-- * PangoColor
	pangoColorParse, pangoColorToString,
	) where

import Graphics.Pango.Basic.TextAttributes.Internal
