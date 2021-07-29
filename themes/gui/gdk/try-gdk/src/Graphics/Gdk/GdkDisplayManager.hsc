{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDisplayManager (
	-- * GDK DISPLAY MANAGER
	GdkDisplayManager,
	-- * FUNCTION
	gdkDisplayManagerGet,
	gdkDisplayManagerGetDefaultDisplay, gdkDisplayManagerSetDefaultDisplay,
	gdkDisplayManagerListDisplays, gdkDisplayManagerOpenDisplay ) where

import Graphics.Gdk.GdkDisplayManager.Internal
