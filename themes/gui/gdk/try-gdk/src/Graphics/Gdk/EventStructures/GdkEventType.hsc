{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures.GdkEventType (
	-- * GDK EVENT TYPE
	GdkEventType,

	-- * MEMBERS
	pattern GdkNothing, pattern GdkDelete, pattern GdkDestroy,
	pattern GdkExpose, pattern GdkMotionNotify,
	pattern GdkButtonPress,
	pattern GdkDoubleButtonPress, pattern GdkTripleButtonPress,
	pattern GdkButtonRelease,
	pattern GdkKeyPress, pattern GdkKeyRelease,
	pattern GdkEnterNotify, pattern GdkLeaveNotify,
	pattern GdkFocusChange, pattern GdkConfigure,
	pattern GdkMap, pattern GdkUnmap,
	pattern GdkPropertyNotify,
	pattern GdkSelectionClear, pattern GdkSelectionRequest,
	pattern GdkSelectionNotify,
	pattern GdkProximityIn, pattern GdkProximityOut,
	pattern GdkDragEnter, pattern GdkDragLeave, pattern GdkDragStatus,
	pattern GdkDropStart, pattern GdkDropFinished,
	pattern GdkClientEvent, pattern GdkVisibilityNotify,
	pattern GdkScroll, pattern GdkWindowState_, pattern GdkSetting,
	pattern GdkOwnerChange, pattern GdkGrabBroken, pattern GdkDamage,
	pattern GdkTouchBegin, pattern GdkTouchEnd, pattern GdkTouchCancel,
	pattern GdkTouchpadSwipe, pattern GdkTouchpadPinch,
	pattern GdkPadButtonPress, pattern GdkPadButtonRelease,
	pattern GdkPadRing, pattern GdkPadStrip, pattern GdkPadGroupMode ) where

import Foreign.Storable
import Foreign.C.Enum
import Data.Int

#include <gdk/gdk.h>

enum "GdkEventType" ''#{type GdkEventType} [''Show, ''Storable] [
	("GdkNothing", #{const GDK_NOTHING}), ("GdkDelete", #{const GDK_DELETE}),
	("GdkDestroy", #{const GDK_DESTROY}), ("GdkExpose", #{const GDK_EXPOSE}),
	("GdkMotionNotify", #{const GDK_MOTION_NOTIFY}),
	("GdkButtonPress", #{const GDK_BUTTON_PRESS}),
--	("Gdk2ButtonPress", #{const GDK_2BUTTON_PRESS}),
	("GdkDoubleButtonPress", #{const GDK_DOUBLE_BUTTON_PRESS}),
--	("Gdk3ButtonPress", #{const GDK_3BUTTON_PRESS}),
	("GdkTripleButtonPress", #{const GDK_TRIPLE_BUTTON_PRESS}),
	("GdkButtonRelease", #{const GDK_BUTTON_RELEASE}),
	("GdkKeyPress", #{const GDK_KEY_PRESS}),
	("GdkKeyRelease", #{const GDK_KEY_RELEASE}),
	("GdkEnterNotify", #{const GDK_ENTER_NOTIFY}),
	("GdkLeaveNotify", #{const GDK_LEAVE_NOTIFY}),
	("GdkFocusChange", #{const GDK_FOCUS_CHANGE}),
	("GdkConfigure", #{const GDK_CONFIGURE}),
	("GdkMap", #{const GDK_MAP}),
	("GdkUnmap", #{const GDK_UNMAP}),
	("GdkPropertyNotify", #{const GDK_PROPERTY_NOTIFY}),
	("GdkSelectionClear", #{const GDK_SELECTION_CLEAR}),
	("GdkSelectionRequest", #{const GDK_SELECTION_REQUEST}),
	("GdkSelectionNotify", #{const GDK_SELECTION_NOTIFY}),
	("GdkProximityIn", #{const GDK_PROXIMITY_IN}),
	("GdkProximityOut", #{const GDK_PROXIMITY_OUT}),
	("GdkDragEnter", #{const GDK_DRAG_ENTER}),
	("GdkDragLeave", #{const GDK_DRAG_LEAVE}),
	("GdkDragMotion", #{const GDK_DRAG_MOTION}),
	("GdkDragStatus", #{const GDK_DRAG_STATUS}),
	("GdkDropStart", #{const GDK_DROP_START}),
	("GdkDropFinished", #{const GDK_DROP_FINISHED}),
	("GdkClientEvent", #{const GDK_CLIENT_EVENT}),
	("GdkVisibilityNotify", #{const GDK_VISIBILITY_NOTIFY}),
	("GdkScroll", #{const GDK_SCROLL}),
	("GdkWindowState_", #{const GDK_WINDOW_STATE}),
	("GdkSetting", #{const GDK_SETTING}),
	("GdkOwnerChange", #{const GDK_OWNER_CHANGE}),
	("GdkGrabBroken", #{const GDK_GRAB_BROKEN}),
	("GdkDamage", #{const GDK_DAMAGE}),
	("GdkTouchBegin", #{const GDK_TOUCH_BEGIN}),
	("GdkTouchUpdate", #{const GDK_TOUCH_UPDATE}),
	("GdkTouchEnd", #{const GDK_TOUCH_END}),
	("GdkTouchCancel", #{const GDK_TOUCH_CANCEL}),
	("GdkTouchpadSwipe", #{const GDK_TOUCHPAD_SWIPE}),
	("GdkTouchpadPinch", #{const GDK_TOUCHPAD_PINCH}),
	("GdkPadButtonPress", #{const GDK_BUTTON_PRESS}),
	("GdkPadButtonRelease", #{const GDK_PAD_BUTTON_RELEASE}),
	("GdkPadRing", #{const GDK_PAD_RING}),
	("GdkPadStrip", #{const GDK_PAD_STRIP}),
	("GdkPadGroupMode", #{const GDK_PAD_GROUP_MODE})
	]
