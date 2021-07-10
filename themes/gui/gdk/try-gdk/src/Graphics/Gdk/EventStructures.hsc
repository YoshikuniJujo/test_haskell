{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.EventStructures where

import GHC.Stack
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int

import {-# SOURCE #-} Graphics.Gdk.Windows

#include <gdk/gdk.h>

enum "GdkEventType" ''#{type GdkEventType} [''Show, ''Storable] [
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

enum "BoolGInt8" ''#{type gint8} [''Show, ''Storable] [
	("False8", #{const FALSE}), ("True8", #{const TRUE}) ]

struct "GdkEventAny" #{size GdkEventAny}
	[	("type", ''GdkEventType, [| #{peek GdkEventAny, type} |],
			[| #{poke GdkEventAny, type} |]),
		("window", ''GdkWindow, [| #{peek GdkEventAny, window} |],
			[| #{poke GdkEventAny, window} |]),
		("sendEvent", ''BoolGInt8,
			[| #{peek GdkEventAny, send_event} |],
			[| #{poke GdkEventAny, send_event} |]) ]
	[''Show]

pattern GdkEventGdkMap :: GdkEventAny -> GdkEvent
pattern GdkEventGdkMap p <- GdkEvent GdkMap (GdkEventAny_ . castForeignPtr -> p)

pattern GdkEventGdkUnmap :: GdkEventAny -> GdkEvent
pattern GdkEventGdkUnmap p <- GdkEvent GdkUnmap (GdkEventAny_ . castForeignPtr -> p)

pattern GdkEventGdkDelete :: GdkEventAny -> GdkEvent
pattern GdkEventGdkDelete p <- GdkEvent GdkDelete (GdkEventAny_ . castForeignPtr -> p)

pattern GdkEventGdkNothing :: GdkEventAny -> GdkEvent
pattern GdkEventGdkNothing p <- GdkEvent GdkNothing (GdkEventAny_ . castForeignPtr -> p)

newtype GdkEventKey = GdkEventKey (ForeignPtr GdkEventKey) deriving Show

pattern GdkEventGdkKeyPress :: GdkEventKey -> GdkEvent
pattern GdkEventGdkKeyPress p <- GdkEvent (GdkEventType #const GDK_KEY_PRESS) (GdkEventKey . castForeignPtr -> p)

gdkEventKeyKeyval :: GdkEventKey -> IO #type guint
gdkEventKeyKeyval (GdkEventKey p) = withForeignPtr p #peek GdkEventKey, keyval

gdkEventKeyWindow :: GdkEventKey -> IO GdkWindow
gdkEventKeyWindow (GdkEventKey p) =
--	GdkWindow <$> (c_g_object_ref =<< withForeignPtr p #peek GdkEventKey, window)
	GdkWindow <$> withForeignPtr p #peek GdkEventKey, window

pattern GdkEventGdkKeyRelease :: GdkEventKey -> GdkEvent
pattern GdkEventGdkKeyRelease p <- GdkEvent (GdkEventType #const GDK_KEY_RELEASE) (GdkEventKey . castForeignPtr -> p)

newtype GdkEventMotion = GdkEventMotion (ForeignPtr GdkEventMotion) deriving Show

pattern GdkEventGdkMotionNotify :: GdkEventMotion -> GdkEvent
pattern GdkEventGdkMotionNotify p <- GdkEvent (GdkEventType #const GDK_MOTION_NOTIFY) (GdkEventMotion . castForeignPtr -> p)

gdkEventMotionX, gdkEventMotionY :: GdkEventMotion -> IO #type gdouble
gdkEventMotionX (GdkEventMotion fm) = withForeignPtr fm #peek GdkEventMotion, x
gdkEventMotionY (GdkEventMotion fm) = withForeignPtr fm #peek GdkEventMotion, y

newtype GdkEventVisibility = GdkEventVisibility (ForeignPtr GdkEventVisibility) deriving Show

pattern GdkEventGdkVisibilityNotify :: GdkEventVisibility -> GdkEvent
pattern GdkEventGdkVisibilityNotify p <-
	GdkEvent (GdkEventType #const GDK_VISIBILITY_NOTIFY) (GdkEventVisibility . castForeignPtr -> p)

gdkEventVisibilityWindow :: GdkEventVisibility -> IO GdkWindow
gdkEventVisibilityWindow (GdkEventVisibility p) =
--	GdkWindow <$> (c_g_object_ref =<< withForeignPtr p #peek GdkEventVisibility, window)
	GdkWindow <$> withForeignPtr p #peek GdkEventVisibility, window

gdkEventFocusWindow :: GdkEventFocus -> IO GdkWindow
gdkEventFocusWindow (GdkEventFocus p) =
	GdkWindow <$> withForeignPtr p #peek GdkEventFocus, window

newtype GdkEventFocus = GdkEventFocus (ForeignPtr GdkEventFocus) deriving Show

pattern GdkEventGdkFocusChange :: GdkEventFocus -> GdkEvent
pattern GdkEventGdkFocusChange p <-
	GdkEvent (GdkEventType #const GDK_FOCUS_CHANGE) (GdkEventFocus . castForeignPtr -> p)

gdkEventFocusIn :: GdkEventFocus -> IO Bool
gdkEventFocusIn (GdkEventFocus p) = gint16ToBool <$> withForeignPtr p #peek GdkEventFocus, in

gint16ToBool :: HasCallStack => #{type gint16} -> Bool
gint16ToBool #{const TRUE} = True
gint16ToBool #{const FALSE} = False
gint16ToBool _ = error "something wrong"

newtype GdkEventConfigure = GdkEventConfigure (ForeignPtr GdkEventConfigure) deriving Show
		
gdkEventConfigureWidth, gdkEventConfigureHeight :: GdkEventConfigure -> IO #type gint
gdkEventConfigureWidth (GdkEventConfigure p) = withForeignPtr p #peek GdkEventConfigure, width
gdkEventConfigureHeight (GdkEventConfigure p) = withForeignPtr p #peek GdkEventConfigure, height

gdkEventConfigureX, gdkEventConfigureY :: GdkEventConfigure -> IO #type gint
gdkEventConfigureX (GdkEventConfigure p) = withForeignPtr p #peek GdkEventConfigure, x
gdkEventConfigureY (GdkEventConfigure p) = withForeignPtr p #peek GdkEventConfigure, y

pattern GdkEventGdkConfigure :: GdkEventConfigure -> GdkEvent
pattern GdkEventGdkConfigure p <- GdkEvent (GdkEventType #{const GDK_CONFIGURE}) (GdkEventConfigure . castForeignPtr -> p)

gdkEventConfigureWindow :: GdkEventConfigure -> IO GdkWindow
gdkEventConfigureWindow (GdkEventConfigure p) = GdkWindow <$> withForeignPtr p #peek GdkEventConfigure, window

newtype GdkEventWindowState = GdkEventWindowState (ForeignPtr GdkEventWindowState) deriving Show

pattern GdkEventGdkWindowState :: GdkEventWindowState -> GdkEvent
pattern GdkEventGdkWindowState p <- GdkEvent (GdkEventType #{const GDK_WINDOW_STATE}) (GdkEventWindowState . castForeignPtr -> p)

enum "GdkScrollDirection" ''#{type GdkScrollDirection} [''Show] [
	("GdkScrollUp", #{const GDK_SCROLL_UP}),
	("GdkScrollDown", #{const GDK_SCROLL_DOWN}),
	("GdkScrollLeft", #{const GDK_SCROLL_LEFT}),
	("GdkScrollRight", #{const GDK_SCROLL_RIGHT}),
	("GdkScrollSmooth", #{const GDK_SCROLL_SMOOTH}) ]

enum "GdkVisibilityState" ''#{type GdkVisibilityState} [''Show] [
	("GdkVisibilityUnobscured", #{const GDK_VISIBILITY_UNOBSCURED}),
	("GdkVisibilityPartial", #{const GDK_VISIBILITY_PARTIAL}),
	("GdkVisibilityFullyObscured", #{const GDK_VISIBILITY_FULLY_OBSCURED}) ]

gdkEventVisibilityState :: GdkEventVisibility -> IO GdkVisibilityState
gdkEventVisibilityState (GdkEventVisibility p) = GdkVisibilityState <$> withForeignPtr p #peek GdkEventVisibility, state

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

gdkEventWindowStateNewWindowState :: GdkEventWindowState -> IO GdkWindowState
gdkEventWindowStateNewWindowState (GdkEventWindowState p) =
	GdkWindowState <$> withForeignPtr p #peek GdkEventWindowState, new_window_state
