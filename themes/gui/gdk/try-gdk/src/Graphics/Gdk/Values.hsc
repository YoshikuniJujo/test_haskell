{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Values where

import Data.Bits
import Data.Word

#include <gdk/gdk.h>

newtype GdkWindowAttributesType = GdkWindowAttributesType #{type GdkWindowAttributesType} deriving Show
#enum GdkWindowAttributesType, GdkWindowAttributesType, \
	GDK_WA_TITLE, GDK_WA_X, GDK_WA_Y, GDK_WA_CURSOR, GDK_WA_VISUAL, \
	GDK_WA_WMCLASS, GDK_WA_NOREDIR, GDK_WA_TYPE_HINT

newtype GdkEventMask = GdkEventMask #{type GdkEventMask} deriving Show

#enum GdkEventMask, GdkEventMask, GDK_EXPOSURE_MASK, \
	GDK_BUTTON_PRESS_MASK, GDK_KEY_PRESS_MASK, \
	GDK_POINTER_MOTION_MASK, \
	GDK_FOCUS_CHANGE_MASK, GDK_ENTER_NOTIFY_MASK, \
	GDK_LEAVE_NOTIFY_MASK, \
	GDK_ALL_EVENTS_MASK

mergeGdkEventMask :: [GdkEventMask] -> #{type GdkEventMask}
mergeGdkEventMask [] = 0
mergeGdkEventMask (GdkEventMask em : ems) = em .|. mergeGdkEventMask ems

newtype GdkWindowType = GdkWindowType #{type GdkWindowType} deriving Show
#enum GdkWindowType, GdkWindowType, GDK_WINDOW_ROOT, GDK_WINDOW_TOPLEVEL, \
	GDK_WINDOW_CHILD

newtype GdkWindowWindowClass = GdkWindowWindowClass #{type GdkWindowWindowClass} deriving Show
#enum GdkWindowWindowClass, GdkWindowWindowClass, GDK_INPUT_OUTPUT, GDK_INPUT_ONLY

newtype GdkWindowState = GdkWindowState #{type GdkWindowState} deriving Show
#enum GdkWindowState, GdkWindowState, GDK_WINDOW_STATE_WITHDRAWN, \
	GDK_WINDOW_STATE_ICONIFIED, GDK_WINDOW_STATE_MAXIMIZED, \
	GDK_WINDOW_STATE_STICKY, GDK_WINDOW_STATE_FULLSCREEN, \
	GDK_WINDOW_STATE_ABOVE, GDK_WINDOW_STATE_BELOW, \
	GDK_WINDOW_STATE_FOCUSED, GDK_WINDOW_STATE_TILED, \
	GDK_WINDOW_STATE_TOP_TILED, GDK_WINDOW_STATE_TOP_RESIZABLE, \
	GDK_WINDOW_STATE_RIGHT_TILED, GDK_WINDOW_STATE_RIGHT_RESIZABLE, \
	GDK_WINDOW_STATE_BOTTOM_TILED, GDK_WINDOW_STATE_BOTTOM_RESIZABLE, \
	GDK_WINDOW_STATE_LEFT_TILED, GDK_WINDOW_STATE_LEFT_RESIZABLE

separateBits :: Bits n => Int -> n -> [n]
separateBits c n = filter (/= zeroBits) $ (\i -> n .&. bit i) <$> [0 .. c - 1]

gdkWindowStateList :: #{type GdkWindowState} -> [GdkWindowState]
gdkWindowStateList = (GdkWindowState <$>) . separateBits 32
