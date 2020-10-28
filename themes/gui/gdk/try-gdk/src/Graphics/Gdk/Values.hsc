{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Values where

import Data.Bits
import Data.Word
import Data.Int

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

newtype GdkVisualType = GdkVisualType #{type GdkVisualType} deriving (Show, Eq)

#enum GdkVisualType, GdkVisualType, GDK_VISUAL_STATIC_GRAY, \
	GDK_VISUAL_GRAYSCALE, GDK_VISUAL_STATIC_COLOR, GDK_VISUAL_PSEUDO_COLOR, \
	GDK_VISUAL_TRUE_COLOR, GDK_VISUAL_DIRECT_COLOR

newtype GdkSeatCapabilities = GdkSeatCapabilities #{type GdkSeatCapabilities} deriving Show

#enum GdkSeatCapabilities, GdkSeatCapabilities, GDK_SEAT_CAPABILITY_NONE, \
	GDK_SEAT_CAPABILITY_POINTER, GDK_SEAT_CAPABILITY_TOUCH, \
	GDK_SEAT_CAPABILITY_TABLET_STYLUS, GDK_SEAT_CAPABILITY_KEYBOARD, \
	GDK_SEAT_CAPABILITY_ALL_POINTING, GDK_SEAT_CAPABILITY_ALL

newtype GdkSubpixelLayout = GdkSubpixelLayout #{type GdkSubpixelLayout} deriving Show

#enum GdkSubpixelLayout, GdkSubpixelLayout, GDK_SUBPIXEL_LAYOUT_UNKNOWN, \
	GDK_SUBPIXEL_LAYOUT_NONE, GDK_SUBPIXEL_LAYOUT_HORIZONTAL_RGB, \
	GDK_SUBPIXEL_LAYOUT_HORIZONTAL_BGR, GDK_SUBPIXEL_LAYOUT_VERTICAL_RGB, \
	GDK_SUBPIXEL_LAYOUT_VERTICAL_BGR

newtype GdkInputSource = GdkInputSource #{type GdkInputSource} deriving Show

#enum GdkInputSource, GdkInputSource, GDK_SOURCE_MOUSE, \
	GDK_SOURCE_PEN, GDK_SOURCE_ERASER, GDK_SOURCE_CURSOR, \
	GDK_SOURCE_KEYBOARD, GDK_SOURCE_TOUCHSCREEN, GDK_SOURCE_TOUCHPAD, \
	GDK_SOURCE_TRACKPOINT, GDK_SOURCE_TABLET_PAD

newtype GdkCursorType = GdkCursorType #{type GdkCursorType} deriving Show

#enum GdkCursorType, GdkCursorType, GDK_X_CURSOR, GDK_ARROW, \
	GDK_BASED_ARROW_DOWN, GDK_BASED_ARROW_UP, GDK_BOAT, GDK_BOGOSITY

newtype GdkWMDecoration = GdkWMDecoration #{type GdkWMDecoration} deriving Show

#enum GdkWMDecoration, GdkWMDecoration, GDK_DECOR_ALL, GDK_DECOR_BORDER, \
	GDK_DECOR_RESIZEH, GDK_DECOR_TITLE, GDK_DECOR_MENU, \
	GDK_DECOR_MINIMIZE, GDK_DECOR_MAXIMIZE
