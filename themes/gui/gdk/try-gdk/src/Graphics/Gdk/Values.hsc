{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Values where

import Foreign.C.Enum
import Data.Bits
import Data.Word

#include <gdk/gdk.h>

newtype GdkWindowWindowClass = GdkWindowWindowClass #{type GdkWindowWindowClass} deriving Show
#enum GdkWindowWindowClass, GdkWindowWindowClass, GDK_INPUT_OUTPUT, GDK_INPUT_ONLY

newtype GdkAxisUse = GdkAxisUse #{type GdkAxisUse} deriving Show

#enum GdkAxisUse, GdkAxisUse, GDK_AXIS_IGNORE, \
	GDK_AXIS_X, GDK_AXIS_Y, GDK_AXIS_PRESSURE, GDK_AXIS_XTILT, \
	GDK_AXIS_YTILT, GDK_AXIS_WHEEL, GDK_AXIS_DISTANCE, GDK_AXIS_ROTATION, \
	GDK_AXIS_SLIDER, GDK_AXIS_LAST

enum "GdkScrollDirection" ''#{type GdkScrollDirection} [''Show] [
	("GdkScrollUp", #{const GDK_SCROLL_UP}),
	("GdkScrollDown", #{const GDK_SCROLL_DOWN}),
	("GdkScrollLeft", #{const GDK_SCROLL_LEFT}),
	("GdkScrollRight", #{const GDK_SCROLL_RIGHT}),
	("GdkScrollSmooth", #{const GDK_SCROLL_SMOOTH}) ]

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
