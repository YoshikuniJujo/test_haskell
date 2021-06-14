{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayout (
	-- * TYPE
	PangoLayout,
	PangoLayoutPrim, PangoLayoutST, PangoLayoutIO,
	pangoLayoutFreeze, pangoLayoutThaw, pangoLayoutCopy,

	-- * CLASS
	-- ** PangoLayoutSetting
	PangoLayoutSetting, pangoLayoutSet, pangoLayoutGet,

	-- ** PangoLayoutInfo
	PangoLayoutInfo, pangoLayoutInfo,

	-- * FUNCTION
	pangoLayoutNew, pangoLayoutContextChanged,
	pangoLayoutSetMarkup, pangoLayoutSetMarkupWithAccel,

	pangoLayoutIndexToPos,
	pangoLayoutIndexToLineX,
	pangoLayoutXyToIndex,
	pangoLayoutGetCursorPos,
	pangoLayoutMoveCursorVisually, Dir(..),

	-- * SETTING

	-- ** Width and Height
	Width(..), Height(..),

	-- ** PangoWrapMode
	PangoWrapMode(..),
	pattern PangoWrapWord, pattern PangoWrapChar, pattern PangoWrapWordChar,

	-- ** PangoEllipsizeMode
	PangoEllipsizeMode(..),
	pattern PangoEllipsizeNone, pattern PangoEllipsizeStart,
	pattern PangoEllipsizeMiddle, pattern PangoEllipsizeEnd,

	-- ** Indent, Spacing, LineSpacing, Justify and AutoDir
	Indent(..), Spacing(..), LineSpacing(..), Justify(..), AutoDir(..),

	-- ** PangoAlignment
	PangoAlignment(..),
	pattern PangoAlignLeft, pattern PangoAlignCenter,
	pattern PangoAlignRight,

	-- ** SingleParagraphMode
	SingleParagraphMode(..),

	-- * INFO

	-- ** CharacterCount, IsWrapped, IsEllipsized and UnknownGlyphCount
	CharacterCount(..), IsWrapped(..), IsEllipsized(..), UnknownGlyphsCount(..),

	-- ** PangoLogAttrs
	PangoLogAttrs, pangoLogAttrsGetLogAttr, pangoLogAttrsGetSize,
	PangoLogAttr,
	pattern PangoLogAttr,
	pangoLogAttrIsLineBreak, pangoLogAttrIsMandatoryBreak,
	pangoLogAttrIsCharBreak, pangoLogAttrIsWhite,
	pangoLogAttrIsCursorPosition,
	pangoLogAttrIsWordStart, pangoLogAttrIsWordEnd,
	pangoLogAttrIsSentenceBoundary,
	pangoLogAttrIsSentenceStart, pangoLogAttrIsSentenceEnd,
	pangoLogAttrBackspaceDeleteCharacter, pangoLogAttrIsExpandableSpace,
	pangoLogAttrIsWordBoundary,

	-- ** LayoutSize, LayoutPixelSize, Baseline and LineCount
	LayoutSize(..), LayoutPixelSize(..), Baseline(..), LineCount(..),

	) where

import Graphics.Pango.Basic.LayoutObjects.PangoLayout.Internal
