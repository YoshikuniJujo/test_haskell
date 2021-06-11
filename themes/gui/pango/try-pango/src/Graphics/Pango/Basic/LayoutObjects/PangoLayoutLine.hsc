{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine (

	-- * TYPE
	PangoLayoutLine,

	-- * GET LINE
	pangoLayoutGetLine, pangoLayoutGetLines,

	-- * GET FROM LINE
	pangoLayoutLineGetExtents, pangoLayoutLineGetPixelExtents,
	pangoLayoutLineIndexToX, pangoLayoutLineXToIndex,
	pangoLayoutLineGetXRanges ) where

import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine.Internal
