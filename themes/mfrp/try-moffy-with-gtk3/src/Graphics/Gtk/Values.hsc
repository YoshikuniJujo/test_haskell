{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk.Values (
	-- * GtkWindowType
	GtkWindowType(..), gtkWindowToplevel, gtkWindowPopup,
	-- * GdkEventMask
	GdkEventMask(..), unifyGdkEventMask,
	gdkExposureMask, gdkPointerMotionMask, gdkPointerMotionHintMask,
	gdkButtonMotionMask, gdkButton1MotionMask, gdkButton2MotionMask, gdkButton3MotionMask,
	gdkButtonPressMask, gdkButtonReleaseMask, gdkKeyPressMask, gdkKeyReleaseMask,
	gdkEnterNotifyMask, gdkLeaveNotifyMask, gdkFocusChangeMask, gdkStructureMask,
	gdkPropertyChangeMask, gdkProximityInMask, gdkProximityOutMask, gdkSubstructureMask,
	gdkScrollMask, gdkTouchpadGestureMask, gdkTabletPadMask, gdkAllEventsMask
	) where

import Data.List
import Data.Bits
import Data.Word
import Data.Int

#include <gtk/gtk.h>

newtype GtkWindowType = GtkWindowType #{type GtkWindowType} deriving Show

#enum GtkWindowType, GtkWindowType, GTK_WINDOW_TOPLEVEL, GTK_WINDOW_POPUP

newtype GdkEventMask = GdkEventMask #{type gint} deriving Show

#enum GdkEventMask, GdkEventMask, \
	GDK_EXPOSURE_MASK, GDK_POINTER_MOTION_MASK, GDK_POINTER_MOTION_HINT_MASK, \
	GDK_BUTTON_MOTION_MASK, GDK_BUTTON1_MOTION_MASK, GDK_BUTTON2_MOTION_MASK, GDK_BUTTON3_MOTION_MASK, \
	GDK_BUTTON_PRESS_MASK, GDK_BUTTON_RELEASE_MASK, GDK_KEY_PRESS_MASK, GDK_KEY_RELEASE_MASK, \
	GDK_ENTER_NOTIFY_MASK, GDK_LEAVE_NOTIFY_MASK, GDK_FOCUS_CHANGE_MASK, GDK_STRUCTURE_MASK, \
	GDK_PROPERTY_CHANGE_MASK, GDK_PROXIMITY_IN_MASK, GDK_PROXIMITY_OUT_MASK, GDK_SUBSTRUCTURE_MASK, \
	GDK_SCROLL_MASK, GDK_TOUCHPAD_GESTURE_MASK, GDK_TABLET_PAD_MASK, GDK_ALL_EVENTS_MASK

unifyGdkEventMask :: [GdkEventMask] -> GdkEventMask
unifyGdkEventMask = GdkEventMask . foldl' (\s (GdkEventMask m) -> s .|. m) 0
