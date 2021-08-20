{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Windows.GdkEventMask (
	-- * GDK EVENT MASK MULTIPLE BITS
	GdkEventMaskMultiBits, gdkEventMaskMultiBits,
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

import Graphics.Gdk.Windows.GdkEventMask.Internal
