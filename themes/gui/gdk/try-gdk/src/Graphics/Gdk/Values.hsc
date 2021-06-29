{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
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
	GDK_BUTTON_PRESS_MASK, GDK_KEY_PRESS_MASK, GDK_BUTTON_RELEASE_MASK, \
	GDK_POINTER_MOTION_MASK, \
	GDK_BUTTON_MOTION_MASK, \
	GDK_FOCUS_CHANGE_MASK, GDK_ENTER_NOTIFY_MASK, \
	GDK_LEAVE_NOTIFY_MASK, \
	GDK_ALL_EVENTS_MASK

mergeGdkEventMask :: [GdkEventMask] -> #{type GdkEventMask}
mergeGdkEventMask [] = 0
mergeGdkEventMask (GdkEventMask em : ems) = em .|. mergeGdkEventMask ems

newtype GdkWindowWindowClass = GdkWindowWindowClass #{type GdkWindowWindowClass} deriving Show
#enum GdkWindowWindowClass, GdkWindowWindowClass, GDK_INPUT_OUTPUT, GDK_INPUT_ONLY

newtype GdkWMDecoration = GdkWMDecoration #{type GdkWMDecoration} deriving Show

#enum GdkWMDecoration, GdkWMDecoration, GDK_DECOR_ALL, GDK_DECOR_BORDER, \
	GDK_DECOR_RESIZEH, GDK_DECOR_TITLE, GDK_DECOR_MENU, \
	GDK_DECOR_MINIMIZE, GDK_DECOR_MAXIMIZE

newtype GdkAxisUse = GdkAxisUse #{type GdkAxisUse} deriving Show

#enum GdkAxisUse, GdkAxisUse, GDK_AXIS_IGNORE, \
	GDK_AXIS_X, GDK_AXIS_Y, GDK_AXIS_PRESSURE, GDK_AXIS_XTILT, \
	GDK_AXIS_YTILT, GDK_AXIS_WHEEL, GDK_AXIS_DISTANCE, GDK_AXIS_ROTATION, \
	GDK_AXIS_SLIDER, GDK_AXIS_LAST

newtype GdkScrollDirection = GdkScrollDirection #{type GdkScrollDirection} deriving Show

#enum GdkScrollDirection, GdkScrollDirection, GDK_SCROLL_UP, \
	GDK_SCROLL_DOWN, GDK_SCROLL_LEFT, GDK_SCROLL_RIGHT, GDK_SCROLL_SMOOTH

toGdkModifierType :: #{type GdkModifierType} -> [GdkModifierType]
toGdkModifierType = (GdkModifierType <$>)
	. filter (/= zeroBits) . (<$> [0 .. 30]) . (\mt n -> mt .&. bit n)

newtype GdkModifierType = GdkModifierType #{type GdkModifierType} deriving Show

#enum GdkModifierType, GdkModifierType, GDK_SHIFT_MASK, \
	GDK_LOCK_MASK, GDK_CONTROL_MASK, GDK_MOD1_MASK, GDK_MOD2_MASK, \
	GDK_MOD3_MASK, GDK_MOD4_MASK, GDK_MOD5_MASK, GDK_BUTTON1_MASK, \
	GDK_BUTTON2_MASK, GDK_BUTTON3_MASK, GDK_BUTTON4_MASK, \
	GDK_BUTTON5_MASK, GDK_SUPER_MASK, GDK_HYPER_MASK, GDK_META_MASK, \
	GDK_MODIFIER_MASK

newtype GdkGrabStatus = GdkGrabStatus #{type GdkGrabStatus} deriving Show

#enum GdkGrabStatus, GdkGrabStatus, GDK_GRAB_SUCCESS, \
	GDK_GRAB_ALREADY_GRABBED, GDK_GRAB_INVALID_TIME, GDK_GRAB_NOT_VIEWABLE, \
	GDK_GRAB_FROZEN, GDK_GRAB_FAILED
