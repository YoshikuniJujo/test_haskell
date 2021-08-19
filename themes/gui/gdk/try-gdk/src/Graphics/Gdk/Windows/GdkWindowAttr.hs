{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.GdkWindowAttr (

	-- * GDK WINDOW ATTRIBUTE
	GdkWindowAttr(..), minimalGdkWindowAttr,

	-- * GDK WINDOW WINDOW CLASS
	GdkWindowWindowClass, pattern GdkInputOutput, pattern GdkInputOnly,

	-- * GDK WINDOW TYPE
	GdkWindowType,
	pattern GdkWindowRoot, pattern GdkWindowToplevel,
	pattern GdkWindowChild, pattern GdkWindowTemp,
	pattern GdkWindowForeign, pattern GdkWindowOffscreen,
	pattern GdkWindowSubsurface ) where

import Graphics.Gdk.Windows.GdkWindowAttr.Internal
