{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Enum
import Data.Word
import Data.Int
import Data.Bits

#include <gdk/gdk.h>

data GdkEvent = GdkEvent GdkEventType (ForeignPtr GdkEvent) deriving Show

newtype GdkEventType = GdkEventType #{type GdkEventType} deriving Show

mkGdkEvent :: Ptr GdkEvent -> IO GdkEvent
mkGdkEvent p = do
	t <- GdkEventType <$> #{peek GdkEvent, type} p
	GdkEvent t <$> newForeignPtr p (c_gdk_event_free p)

foreign import ccall "gdk_event_free" c_gdk_event_free :: Ptr GdkEvent -> IO ()

#enum GdkEventType, GdkEventType, \
	GDK_NOTHING, GDK_DELETE, GDK_DESTROY, GDK_EXPOSE, GDK_MOTION_NOTIFY, \
	GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_DOUBLE_BUTTON_PRESS, \
	GDK_3BUTTON_PRESS, GDK_TRIPLE_BUTTON_PRESS, GDK_BUTTON_RELEASE, \
	GDK_KEY_PRESS, GDK_KEY_RELEASE, GDK_ENTER_NOTIFY, GDK_LEAVE_NOTIFY, \
	GDK_FOCUS_CHANGE, GDK_CONFIGURE, GDK_MAP, GDK_UNMAP, GDK_PROPERTY_NOTIFY, \
	GDK_SELECTION_CLEAR, GDK_SELECTION_REQUEST, GDK_SELECTION_NOTIFY, \
	GDK_PROXIMITY_IN, GDK_PROXIMITY_OUT, GDK_DRAG_ENTER, GDK_DRAG_LEAVE, \
	GDK_DRAG_MOTION, GDK_DRAG_STATUS, GDK_DROP_START, GDK_DROP_FINISHED, \
	GDK_CLIENT_EVENT, GDK_VISIBILITY_NOTIFY, GDK_SCROLL, GDK_WINDOW_STATE, \
	GDK_SETTING, GDK_OWNER_CHANGE, GDK_GRAB_BROKEN

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
