{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Values where

import Foreign.C.Enum
import Data.Bits
import Data.Word

#include <gdk/gdk.h>

enum "GdkEventMaskSingleBit" ''#{type GdkEventMask} [''Show] [
	("GdkExposureMask", #{const GDK_EXPOSURE_MASK}),
	("GdkPointerMotionMask", #{const GDK_POINTER_MOTION_MASK}),
	("GdkButtonMotionMask", #{const GDK_BUTTON_MOTION_MASK}),
	("GdkButton1MotionMask", #{const GDK_BUTTON1_MOTION_MASK}),
	("GdkButton2MotionMask", #{const GDK_BUTTON2_MOTION_MASK}),
	("GdkButton3MotionMask", #{const GDK_BUTTON3_MOTION_MASK}),
	("GdkButtonPressMask", #{const GDK_BUTTON_PRESS_MASK}),
	("GdkButtonReleaseMask", #{const GDK_BUTTON_RELEASE_MASK}),
	("GdkKeyPressMask", #{const GDK_KEY_PRESS_MASK}),
	("GdkKeyReleaseMask", #{const GDK_KEY_RELEASE_MASK}),
	("GdkEnterNotifyMask", #{const GDK_ENTER_NOTIFY_MASK}),
	("GdkLeaveNotifyMask", #{const GDK_LEAVE_NOTIFY_MASK}),
	("GdkFocusChangeMask", #{const GDK_FOCUS_CHANGE_MASK}),
	("GdkStructureMask", #{const GDK_STRUCTURE_MASK}),
	("GdkPropertyChangeMask", #{const GDK_PROPERTY_CHANGE_MASK}),
	("GdkVisibilityNotifyMask", #{const GDK_VISIBILITY_NOTIFY_MASK}),
	("GdkProximityInMask", #{const GDK_PROXIMITY_IN_MASK}),
	("GdkProximityOutMask", #{const GDK_PROXIMITY_OUT_MASK}),
	("GdkSubstructureMask", #{const GDK_SUBSTRUCTURE_MASK}),
	("GdkScrollMssk", #{const GDK_SCROLL_MASK}),
	("GdkTouchMask", #{const GDK_TOUCH_MASK}),
	("GdkSmoothScrollMask", #{const GDK_SMOOTH_SCROLL_MASK}),
	("GdkTouchpadGestureMask", #{const GDK_TOUCHPAD_GESTURE_MASK}),
	("GdkTabletPadMask", #{const GDK_TABLET_PAD_MASK}) ]

enum "GdkEventMaskMultiBits" ''#{type GdkEventMask} [''Show] [
	("GdkZeroEventsMask", 0),
	("GdkAllEventsMask", #{const GDK_ALL_EVENTS_MASK}) ]

getGdkEventMask :: GdkEventMaskMultiBits -> #{type GdkEventMask}
getGdkEventMask (GdkEventMaskMultiBits em) = em

gdkEventMaskMultiBits :: [GdkEventMaskSingleBit] -> GdkEventMaskMultiBits
gdkEventMaskMultiBits = GdkEventMaskMultiBits . mergeGdkEventMask

mergeGdkEventMask :: [GdkEventMaskSingleBit] -> #{type GdkEventMask}
mergeGdkEventMask [] = 0
mergeGdkEventMask (GdkEventMaskSingleBit em : ems) = em .|. mergeGdkEventMask ems

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
