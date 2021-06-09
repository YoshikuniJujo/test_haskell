{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage (
	-- * PANGO FIXED
	PangoFixed, PU, fromCInt, toCInt,

	-- * PANGO RECTANGLE AND EXTENTS
	-- ** PangoRectangleFixed
	PangoRectangleFixed,
	pattern PangoRectangleFixed,
	pangoRectangleFixedX, pangoRectangleFixedY,
	pangoRectangleFixedWidth, pangoRectangleFixedHeight,

	PangoRectangleFixedPrim, PangoRectangleFixedST, PangoRectangleFixedIO,
	pangoRectangleFixedFreeze, pangoRectangleFixedThaw, pangoRectangleFixedCopy,

	-- ** PangoRectanglePixel
	PangoRectanglePixel,
	pattern PangoRectanglePixel,
	pangoRectanglePixelX, pangoRectanglePixelY,
	pangoRectanglePixelWidth, pangoRectanglePixelHeight,

	PangoRectanglePixelPrim,
	PangoRectanglePixelST, PangoRectanglePixelIO,
	pangoRectanglePixelFreeze, pangoRectanglePixelThaw,
	pangoRectanglePixelCopy,

	-- ** Extents and PixelExtents
	Extents(..), PixelExtents(..),

	-- * PANGO GLYPH ITEM
	PangoGlyphItem, PangoLayoutRun,
	) where

import Graphics.Pango.Basic.GlyphStorage.Internal
