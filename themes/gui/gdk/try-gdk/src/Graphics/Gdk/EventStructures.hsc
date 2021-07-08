{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int

#include <gdk/gdk.h>

enum "GdkEventType" ''#{type GdkEventType} [''Show] [
	("GdkNothing", #{const GDK_NOTHING}), ("GdkDelete", #{const GDK_DELETE}),
	("GdkExpose", #{const GDK_EXPOSE}),
	("GdkMotionNotify", #{const GDK_MOTION_NOTIFY}),
	("GdkButtonPress", #{const GDK_BUTTON_PRESS}),
	("Gdk2ButtonPress", #{const GDK_2BUTTON_PRESS}),
	("GdkDoubleButtonPress", #{const GDK_DOUBLE_BUTTON_PRESS}),
	("Gdk3ButtonPress", #{const GDK_TRIPLE_BUTTON_PRESS}),
	("GdkButtonRelesae", #{const GDK_BUTTON_RELEASE}),
	("GdkKeyPress", #{const GDK_KEY_PRESS}),
	("GdkKeyRelease", #{const GDK_KEY_RELEASE}),
	("GdkEnterNotify", #{const GDK_ENTER_NOTIFY}),
	("GdkLeaveNotify", #{const GDK_LEAVE_NOTIFY}),
	("GdkFocusChange", #{const GDK_FOCUS_CHANGE}),
	("GdkConfiguyre", #{const GDK_CONFIGURE}),
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
	("GdkGrabBroken", #{const GDK_GRAB_BROKEN})
	]

data GdkEvent = GdkEvent GdkEventType (ForeignPtr GdkEvent) deriving Show

mkGdkEvent :: Ptr GdkEvent -> IO GdkEvent
mkGdkEvent p = do
	t <- GdkEventType <$> #{peek GdkEvent, type} p
	GdkEvent t <$> newForeignPtr p (c_gdk_event_free p)

foreign import ccall "gdk_event_free" c_gdk_event_free :: Ptr GdkEvent -> IO ()

newtype GdkWindowStates = GdkWindowStates #{type GdkWindowState} deriving Show

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

gdkWindowStateCheck :: GdkWindowState -> GdkWindowStates -> Bool
gdkWindowStateCheck (GdkWindowState s) (GdkWindowStates ss) = s .&. ss /= zeroBits

gdkWindowStateList :: GdkWindowStates -> [GdkWindowState]
gdkWindowStateList (GdkWindowStates ss) = GdkWindowState <$> separateBits 32 ss
