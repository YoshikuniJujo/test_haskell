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
	pattern GdkWindowSubsurface,

	-- * GDK WINDOW TYPE HINT
	GdkWindowTypeHint,
	pattern GdkWindowTypeHintNormal, pattern GdkWindowTypeHintDialog,
	pattern GdkWindowTypeHintMenu, pattern GdkWindowTypeHintToolbar,
	pattern GdkWindowTypeHintSplashscreen, pattern GdkWindowTypeHintUtility,
	pattern GdkWindowTypeHintDock, pattern GdkWindowTypeHintDesktop,
	pattern GdkWindowTypeHintDropdownMenu, pattern GdkWindowTypeHintPopupMenu,
	pattern GdkWindowTypeHintTooltip, pattern GdkWindowTypeHintNotification,
	pattern GdkWindowTypeHintCombo, pattern GdkWindowTypeHintDnd ) where

import Graphics.Gdk.Windows.GdkWindowAttr.Internal
