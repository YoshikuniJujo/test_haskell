{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Event where

import Foreign.Ptr
import Foreign.Storable
import Control.Exception
import Data.Bool
import Data.Word
import Data.Int

import Graphics.Gdk

#include <gdk/gdk.h>

data GdkEvent = GdkEvent GdkEventType (Ptr GdkEvent) deriving Show

newtype GdkEventType = GdkEventType #{type GdkEventType} deriving Show

makeGdkEvent :: Ptr GdkEvent -> IO GdkEvent
makeGdkEvent p = do
	t <- GdkEventType <$> #{peek GdkEvent, type} p
	pure $ GdkEvent t p

#enum GdkEventType, GdkEventType, \
	GDK_NOTHING, GDK_DELETE, GDK_DESTROY, GDK_EXPOSE, GDK_MOTION_NOTIFY, \
	GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_DOUBLE_BUTTON_PRESS, \
	GDK_3BUTTON_PRESS, GDK_TRIPLE_BUTTON_PRESS, GDK_BUTTON_RELEASE, \
	GDK_KEY_PRESS, GDK_KEY_RELEASE, GDK_ENTER_NOTIFY, GDK_LEAVE_NOTIFY, \
	GDK_FOCUS_CHANGE, GDK_CONFIGURE, GDK_MAP, GDK_UNMAP, GDK_PROPERTY_NOTIFY, \
	GDK_SELECTION_CLEAR, GDK_SELECTION_REQUEST, GDK_SELECTION_NOTIFY, \
	GDK_PROXIMITY_IN, GDK_PROXIMITY_OUT, GDK_DRAG_ENTER, GDK_DRAG_LEAVE, \
	GDK_DRAG_MOTION, GDK_DRAG_STATUS, GDK_DROP_START, GDK_DROP_FINISHED, \
	GDK_CLIENT_EVENT, GDK_VISIBILITY_NOTIFY, GDK_SCROLL, GDK_WINDOW_STATE, \
	GDK_SETTING, GDK_OWNER_CHANGE, GDK_GRAB_BROKEN

foreign import ccall "gdk_event_get" c_gdk_event_get :: IO (Ptr GdkEvent)
foreign import ccall "gdk_events_pending" c_gdk_event_pending :: IO #type gboolean

gdkWithEvent :: (Maybe GdkEvent -> IO a) -> IO a
gdkWithEvent f = bracket
	c_gdk_event_get
	(\p -> bool (c_gdk_event_free p) (pure ()) $ p == nullPtr)
	(\p -> f =<< bool (Just <$> makeGdkEvent p) (pure Nothing) (p == nullPtr))

foreign import ccall "gdk_event_free" c_gdk_event_free :: Ptr GdkEvent -> IO ()

newtype GdkEventConfigure = GdkEventConfigure (Ptr GdkEventConfigure) deriving Show
		
gdkEventConfigureWidth, gdkEventConfigureHeight :: GdkEventConfigure -> IO #type gint
gdkEventConfigureWidth (GdkEventConfigure p) = #{peek GdkEventConfigure, width} p
gdkEventConfigureHeight (GdkEventConfigure p) = #{peek GdkEventConfigure, height} p

pattern GdkEventGdkConfigure :: GdkEventConfigure -> GdkEvent
pattern GdkEventGdkConfigure p <- GdkEvent (GdkEventType #{const GDK_CONFIGURE}) (GdkEventConfigure . castPtr -> p)

newtype GdkEventWindowState = GdkEventWindowState (Ptr GdkEventWindowState) deriving Show

pattern GdkEventGdkWindowState :: GdkEventWindowState -> GdkEvent
pattern GdkEventGdkWindowState p <- GdkEvent (GdkEventType #{const GDK_WINDOW_STATE}) (GdkEventWindowState . castPtr -> p)

newtype GdkWindowState = GdkWindowState #{type GdkWindowState} deriving Show

#enum GdkWindowState, GdkWindowState, GDK_WINDOW_STATE_WITHDRAWN, \
	GDK_WINDOW_STATE_ICONIFIED

gdkEventWindowStateNewWindowState :: GdkEventWindowState -> IO GdkWindowState
gdkEventWindowStateNewWindowState (GdkEventWindowState p) =
	GdkWindowState <$> #{peek GdkEventWindowState, new_window_state} p

newtype GdkEventAny = GdkEventAny (Ptr GdkEventAny) deriving Show

pattern GdkEventGdkMap :: GdkEventAny -> GdkEvent
pattern GdkEventGdkMap p <- GdkEvent (GdkEventType #const GDK_MAP) (GdkEventAny . castPtr -> p)

newtype GdkEventVisibility = GdkEventVisibility (Ptr GdkEventVisibility) deriving Show

pattern GdkEventGdkVisibilityNotify :: GdkEventVisibility -> GdkEvent
pattern GdkEventGdkVisibilityNotify p <-
	GdkEvent (GdkEventType #const GDK_VISIBILITY_NOTIFY) (GdkEventVisibility . castPtr -> p)

newtype GdkVisibilityState = GdkVisibilityState #{type GdkVisibilityState} deriving Show

#enum GdkVisibilityState, GdkVisibilityState, GDK_VISIBILITY_UNOBSCURED, \
	GDK_VISIBILITY_PARTIAL, GDK_VISIBILITY_FULLY_OBSCURED

gdkEventVisibilityState :: GdkEventVisibility -> IO GdkVisibilityState
gdkEventVisibilityState (GdkEventVisibility p) = GdkVisibilityState <$> #{peek GdkEventVisibility, state} p
