{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Visuals (
	-- * TYPE
	GdkVisual,

	-- * FUNCTION
	gdkVisualGetDepth,
	gdkVisualGetRedPixelDetails,
	gdkVisualGetGreenPixelDetails,
	gdkVisualGetBluePixelDetails,
	gdkVisualGetScreen,

	-- * GDK VISUAL TYPE
	GdkVisualType,
	gdkVisualGetVisualType,
	pattern GdkVisualStaticGray, pattern GdkVisualGrayscale,
	pattern GdkVisualStaticColor, pattern GdkVisualPseudoColor,
	pattern GdkVisualTrueColor, pattern GdkVisualDirectColor
) where

import Graphics.Gdk.Visuals.Internal
