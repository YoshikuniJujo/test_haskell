{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures where

import Foreign.C.Enum
import Data.Word
import Data.Bits

#include <gdk/gdk.h>

enum "GdkWindowState" ''#{type GdkWindowState} [''Show] [
	("GdkWindowStateWithdrawn", #{const GDK_WINDOW_STATE_WITHDRAWN}),
	("GdkWindowStateIconified", #{const GDK_WINDOW_STATE_ICONIFIED}),
	("GdkWindowStateMaximized", #{const GDK_WINDOW_STATE_MAXIMIZED}),
	("GdkWindowStateSticky", #{const GDK_WINDOW_STATE_STICKY}),
	("GdkWindowStateFullscreen", #{const GDK_WINDOW_STATE_FULLSCREEN}),
	("GdkWindowStateAbove", #{const GDK_WINDOW_STATE_ABOVE}),
	("GdkWindowStateBelow", #{const GDK_WINDOW_STATE_BELOW}),
	("GdkWindowStateFocused", #{const GDK_WINDOW_STATE_FOCUSED}),
	("GdkWindowStateTopTiled", #{const GDK_WINDOW_STATE_TOP_TILED}),
	("GdkWidnwoStateTopResizable", #{const GDK_WINDOW_STATE_TOP_RESIZABLE}),
	("GdkWindowStateRightTiled", #{const GDK_WINDOW_STATE_RIGHT_TILED}),
	("GdkWindowStateRightResizable",
		#{const GDK_WINDOW_STATE_RIGHT_RESIZABLE}),
	("GdkWindowStateBottomTiled", #{const GDK_WINDOW_STATE_BOTTOM_TILED}),
	("GdkWindowStaetBottomResizable",
		#{const GDK_WINDOW_STATE_BOTTOM_RESIZABLE}),
	("GdkWindowStateLeftTiled", #{const GDK_WINDOW_STATE_LEFT_TILED}),
	("GdkWindowStateLeftResizable",
		#{const GDK_WINDOW_STATE_LEFT_RESIZABLE}) ]

gdkWindowStateList :: #{type GdkWindowState} -> [GdkWindowState]
gdkWindowStateList = (GdkWindowState <$>) . separateBits 32

separateBits :: Bits n => Int -> n -> [n]
separateBits c n = filter (/= zeroBits) $ (\i -> n .&. bit i) <$> [0 .. c - 1]
