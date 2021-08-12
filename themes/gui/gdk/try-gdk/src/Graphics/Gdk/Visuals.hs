{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Visuals (
	-- * GDK VISUAL
	GdkVisual,

	-- * SCREEN
	gdkVisualGetScreen,

	-- * GDK VISUAL TYPE
	gdkVisualGetVisualType,
	GdkVisualType,
	pattern GdkVisualStaticGray, pattern GdkVisualGrayscale,
	pattern GdkVisualStaticColor, pattern GdkVisualPseudoColor,
	pattern GdkVisualTrueColor, pattern GdkVisualDirectColor,

	-- * DEPTH
	gdkVisualGetDepth,

	-- * RED, GREEN AND BLUE
	gdkVisualGetRedPixelDetails,
	gdkVisualGetGreenPixelDetails,
	gdkVisualGetBluePixelDetails ) where

import Graphics.Gdk.Visuals.Internal
