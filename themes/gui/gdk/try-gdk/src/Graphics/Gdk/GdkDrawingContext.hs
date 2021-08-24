{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDrawingContext (

	-- * GDK DRAWING CONTEXT
	GdkDrawingContext,

	-- * IS VALID
	gdkDrawingContextIsValid,

	-- * WINDOW AND CLIP
	gdkDrawingContextGetWindow, gdkDrawingContextGetClip,

	-- * CAIRO CONTEXT
	gdkDrawingContextGetCairoContext ) where

import Graphics.Gdk.GdkDrawingContext.Internal
