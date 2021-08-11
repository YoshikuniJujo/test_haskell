{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.PointsAndRectangles (
	-- * GDK RECTANGLE
	GdkRectangle,
	pattern GdkRectangle,
	gdkRectangleX, gdkRectangleY, gdkRectangleWidth, gdkRectangleHeight,

	-- * GDK RECTANGLE PRIM
	GdkRectanglePrim, GdkRectangleIO, GdkRectangleST,
	gdkRectangleNew, gdkRectangleFreeze, gdkRectangleThaw, gdkRectangleCopy,

	-- * FUNCTION
	gdkRectangleIntersect, gdkRectangleUnion, gdkRectangleEqual,
	) where

import Graphics.Gdk.PointsAndRectangles.Internal
