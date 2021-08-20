{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.GdkEventMask.Internal (
	-- * GDK EVENT MASK MULTIPLE BITS
	GdkEventMaskMultiBits(..), gdkEventMaskMultiBits,
	pattern GdkZeroEventsMask, pattern GdkAllEventsMask,

	-- * GDK EVENT MASK SINGLE BIT
	GdkEventMaskSingleBit, gdkEventMaskSingleBitList,
	pattern GdkExposureMask, pattern GdkPointerMotionMask,
	pattern GdkButtonMotionMask, pattern GdkButton1MotionMask,
	pattern GdkButton2MotionMask, pattern GdkButton3MotionMask,
	pattern GdkButtonPressMask, pattern GdkButtonReleaseMask,
	pattern GdkKeyPressMask, pattern GdkKeyReleaseMask,
	pattern GdkEnterNotifyMask, pattern GdkLeaveNotifyMask,
	pattern GdkFocusChangeMask, pattern GdkStructureMask,
	pattern GdkPropertyChangeMask, pattern GdkVisibilityNotifyMask,
	pattern GdkProximityInMask, pattern GdkProximityOutMask,
	pattern GdkSubstructureMask, pattern GdkScrollMask,
	pattern GdkTouchMask, pattern GdkSmoothScrollMask,
	pattern GdkTouchpadGestureMask, pattern GdkTabletPadMask ) where

import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Word

#include <gdk/gdk.h>

enum "GdkEventMaskSingleBit" ''#{type GdkEventMask} [''Show, ''Read, ''Eq] [
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
	("GdkScrollMask", #{const GDK_SCROLL_MASK}),
	("GdkTouchMask", #{const GDK_TOUCH_MASK}),
	("GdkSmoothScrollMask", #{const GDK_SMOOTH_SCROLL_MASK}),
	("GdkTouchpadGestureMask", #{const GDK_TOUCHPAD_GESTURE_MASK}),
	("GdkTabletPadMask", #{const GDK_TABLET_PAD_MASK}) ]

enum "GdkEventMaskMultiBits" ''#{type GdkEventMask} [''Show, ''Read, ''Eq] [
	("GdkZeroEventsMask", 0),
	("GdkAllEventsMask", #{const GDK_ALL_EVENTS_MASK}) ]

gdkEventMaskMultiBits :: [GdkEventMaskSingleBit] -> GdkEventMaskMultiBits
gdkEventMaskMultiBits = GdkEventMaskMultiBits . mergeGdkEventMask

gdkEventMaskSingleBitList :: GdkEventMaskMultiBits -> [GdkEventMaskSingleBit]
gdkEventMaskSingleBitList (GdkEventMaskMultiBits ems) =
	GdkEventMaskSingleBit <$> separateBits (#{size GdkEventMask} * 8) ems

mergeGdkEventMask :: [GdkEventMaskSingleBit] -> #{type GdkEventMask}
mergeGdkEventMask [] = 0
mergeGdkEventMask (GdkEventMaskSingleBit em : ems) = em .|. mergeGdkEventMask ems
