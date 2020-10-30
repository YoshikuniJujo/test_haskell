{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Events where

import GHC.Stack
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Data.Bool
import Data.Word
import Data.Int

import Graphics.Gdk.Types
import Graphics.Gdk.Values

#include <gdk/gdk.h>

data GdkEvent = GdkEvent GdkEventType (ForeignPtr GdkEvent) deriving Show

newtype GdkEventType = GdkEventType #{type GdkEventType} deriving Show

mkGdkEvent :: Ptr GdkEvent -> IO GdkEvent
mkGdkEvent p = do
	t <- GdkEventType <$> #{peek GdkEvent, type} p
	GdkEvent t <$> newForeignPtr p (c_gdk_event_free p)

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

foreign import ccall "gdk_events_pending" c_gdk_events_pending :: IO #type gboolean

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool _ = True

gdkEventsPending :: IO Bool
gdkEventsPending = gbooleanToBool <$> c_gdk_events_pending

foreign import ccall "gdk_event_peek" c_gdk_event_peek :: IO (Ptr GdkEvent)

gdkEventPeek :: IO GdkEvent
gdkEventPeek = mkGdkEvent =<< c_gdk_event_peek

foreign import ccall "gdk_event_get" c_gdk_event_get :: IO (Ptr GdkEvent)

gdkEventGet :: IO (Maybe GdkEvent)
gdkEventGet = do
	p <- c_gdk_event_get
	if p == nullPtr
		then pure Nothing
		else Just <$> mkGdkEvent p

foreign import ccall "gdk_event_put" c_gdk_event_put :: Ptr GdkEvent -> IO ()

gdkEventPut :: GdkEvent -> IO ()
gdkEventPut (GdkEvent _ fe) = withForeignPtr fe c_gdk_event_put

foreign import ccall "gdk_event_new" c_gdk_event_new :: IO (Ptr GdkEvent)

gdkEventNew :: IO GdkEvent
gdkEventNew = mkGdkEvent =<< c_gdk_event_new

foreign import ccall "gdk_event_copy" c_gdk_event_copy :: Ptr GdkEvent -> IO (Ptr GdkEvent)

gdkEventCopy :: GdkEvent -> IO GdkEvent
gdkEventCopy (GdkEvent _ fe) = withForeignPtr fe \e -> mkGdkEvent =<< c_gdk_event_copy e

foreign import ccall "gdk_event_get_axis" c_gdk_event_get_axis ::
	Ptr GdkEvent -> #{type GdkAxisUse} -> Ptr #{type gdouble} -> IO #type gboolean

gdkEventGetAxis :: GdkEvent -> GdkAxisUse -> IO (Maybe #type gdouble)
gdkEventGetAxis (GdkEvent _ fe) (GdkAxisUse ax) =
	withForeignPtr fe \e -> alloca \v -> c_gdk_event_get_axis e ax v >>=
		bool (pure Nothing) (Just <$> peek v) . gbooleanToBool

foreign import ccall "gdk_event_get_button" c_gdk_event_get_button ::
	Ptr GdkEvent -> Ptr #{type guint} -> IO #type gboolean

gdkEventGetButton :: GdkEvent -> IO (Maybe #type guint)
gdkEventGetButton (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \b ->
	c_gdk_event_get_button e b >>= bool (pure Nothing) (Just <$> peek b) . gbooleanToBool

foreign import ccall "gdk_event_get_click_count" c_gdk_event_get_click_count ::
	Ptr GdkEvent -> Ptr #{type guint} -> IO #type gboolean

gdkEventGetClickCount :: GdkEvent -> IO (Maybe #type guint)
gdkEventGetClickCount (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \cc ->
	c_gdk_event_get_click_count e cc
		>>= bool (pure Nothing) (Just <$> peek cc) . gbooleanToBool

foreign import ccall "gdk_event_get_coords" c_gdk_event_get_coords ::
	Ptr GdkEvent -> Ptr #{type gdouble} -> Ptr #{type gdouble} -> IO #type gboolean

gdkEventGetCoords :: GdkEvent -> IO (Maybe (#{type gdouble}, #type gdouble))
gdkEventGetCoords (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \xw -> alloca \yw ->
	c_gdk_event_get_coords e xw yw
		>>= bool (pure Nothing) ((\x y -> Just (x, y)) <$> peek xw <*> peek yw) . gbooleanToBool

foreign import ccall "gdk_event_get_keycode" c_gdk_event_get_keycode ::
	Ptr GdkEvent -> Ptr #{type guint16} -> IO #type gboolean

gdkEventGetKeycode :: GdkEvent -> IO (Maybe #type guint16)
gdkEventGetKeycode (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \kc ->
	c_gdk_event_get_keycode e kc
		>>= bool (pure Nothing) (Just <$> peek kc) . gbooleanToBool

foreign import ccall "gdk_event_get_keyval" c_gdk_event_get_keyval ::
	Ptr GdkEvent -> Ptr #{type guint} -> IO #type gboolean

gdkEventGetKeyval :: GdkEvent -> IO (Maybe #type guint)
gdkEventGetKeyval (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \kv ->
	c_gdk_event_get_keyval e kv
		>>= bool (pure Nothing) (Just <$> peek kv) . gbooleanToBool

foreign import ccall "gdk_event_get_root_coords" c_gdk_event_get_root_coords ::
	Ptr GdkEvent -> Ptr #{type gdouble} -> Ptr #{type gdouble} -> IO #type gboolean

gdkEventGetRootCoords :: GdkEvent -> IO (Maybe (#{type gdouble}, #type gdouble))
gdkEventGetRootCoords (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \xr -> alloca \yr ->
	c_gdk_event_get_root_coords e xr yr
		>>= bool (pure Nothing) ((\x y -> Just (x, y)) <$> peek xr <*> peek yr) . gbooleanToBool

foreign import ccall "gdk_event_get_scroll_direction" c_gdk_event_get_scroll_direction ::
	Ptr GdkEvent -> Ptr #{type GdkScrollDirection} -> IO #type gboolean

gdkEventGetScrollDirection :: GdkEvent -> IO (Maybe GdkScrollDirection)
gdkEventGetScrollDirection (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \sd ->
	c_gdk_event_get_scroll_direction e sd
		>>= bool (pure Nothing) (Just . GdkScrollDirection <$> peek sd) . gbooleanToBool

foreign import ccall "gdk_event_get_scroll_deltas" c_gdk_event_get_scroll_deltas ::
	Ptr GdkEvent -> Ptr #{type gdouble} -> Ptr #{type gdouble} -> IO #type gboolean

gdkEventGetScrollDeltas :: GdkEvent -> IO (Maybe (#{type gdouble}, #type gdouble))
gdkEventGetScrollDeltas (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \dx -> alloca \dy ->
	c_gdk_event_get_scroll_deltas e dx dy
		>>= bool (pure Nothing) ((\x y -> Just (x, y)) <$> peek dy <*> peek dy) . gbooleanToBool

foreign import ccall "gdk_event_is_scroll_stop_event" c_gdk_event_is_scroll_stop_event ::
	Ptr GdkEvent -> IO #type gboolean

gdkEventIsScrollStopEvent :: GdkEvent -> IO Bool
gdkEventIsScrollStopEvent (GdkEvent _ fe) = withForeignPtr fe \e ->
	gbooleanToBool <$> c_gdk_event_is_scroll_stop_event e

foreign import ccall "gdk_event_get_state" c_gdk_event_get_state ::
	Ptr GdkEvent -> Ptr #{type GdkModifierType} -> IO #type gboolean

gdkEventGetState :: GdkEvent -> IO (Maybe [GdkModifierType])
gdkEventGetState (GdkEvent _ fe) = withForeignPtr fe \e -> alloca \mt -> do
	c_gdk_event_get_state e mt
		>>= bool (pure Nothing) (Just . toGdkModifierType <$> peek mt) . gbooleanToBool

foreign import ccall "gdk_event_get_time" c_gdk_event_get_time ::
	Ptr GdkEvent -> IO #type guint32

gdkEventGetTime :: GdkEvent -> IO (Maybe #type guint32)
gdkEventGetTime (GdkEvent _ fe) = withForeignPtr fe \e -> (<$> c_gdk_event_get_time e) \case
	t	| t == #{const GDK_CURRENT_TIME} -> Nothing
		| otherwise -> Just t

foreign import ccall "gdk_event_get_window" c_gdk_event_get_window ::
	Ptr GdkEvent -> IO (Ptr GdkWindow)

gdkEventGetWindow :: GdkEvent -> IO GdkWindow
gdkEventGetWindow (GdkEvent _ fe) = withForeignPtr fe \e ->
	GdkWindow <$> c_gdk_event_get_window e

foreign import ccall "gdk_event_get_event_type" c_gdk_event_get_event_type ::
	Ptr GdkEvent -> IO #type GdkEventType

gdkEventGetEventType :: GdkEvent -> IO GdkEventType
gdkEventGetEventType (GdkEvent _ fe) = withForeignPtr fe \e ->
	GdkEventType <$> c_gdk_event_get_event_type e

foreign import ccall "gdk_event_get_seat" c_gdk_event_get_seat ::
	Ptr GdkEvent -> IO (Ptr GdkSeat)

gdkEventGetSeat :: GdkEvent -> IO GdkSeat
gdkEventGetSeat (GdkEvent _ fe) = withForeignPtr fe \e ->
	GdkSeat <$> c_gdk_event_get_seat e

foreign import ccall "gdk_event_free" c_gdk_event_free :: Ptr GdkEvent -> IO ()

newtype GdkEventConfigure = GdkEventConfigure (ForeignPtr GdkEventConfigure) deriving Show
		
gdkEventConfigureWidth, gdkEventConfigureHeight :: GdkEventConfigure -> IO #type gint
gdkEventConfigureWidth (GdkEventConfigure p) = withForeignPtr p #peek GdkEventConfigure, width
gdkEventConfigureHeight (GdkEventConfigure p) = withForeignPtr p #peek GdkEventConfigure, height

gdkEventConfigureX, gdkEventConfigureY :: GdkEventConfigure -> IO #type gint
gdkEventConfigureX (GdkEventConfigure p) = withForeignPtr p #peek GdkEventConfigure, x
gdkEventConfigureY (GdkEventConfigure p) = withForeignPtr p #peek GdkEventConfigure, y

pattern GdkEventGdkConfigure :: GdkEventConfigure -> GdkEvent
pattern GdkEventGdkConfigure p <- GdkEvent (GdkEventType #{const GDK_CONFIGURE}) (GdkEventConfigure . castForeignPtr -> p)

newtype GdkEventWindowState = GdkEventWindowState (ForeignPtr GdkEventWindowState) deriving Show

pattern GdkEventGdkWindowState :: GdkEventWindowState -> GdkEvent
pattern GdkEventGdkWindowState p <- GdkEvent (GdkEventType #{const GDK_WINDOW_STATE}) (GdkEventWindowState . castForeignPtr -> p)

gdkEventWindowStateNewWindowState :: GdkEventWindowState -> IO GdkWindowState
gdkEventWindowStateNewWindowState (GdkEventWindowState p) =
	GdkWindowState <$> withForeignPtr p #peek GdkEventWindowState, new_window_state

data GdkEventAny = GdkEventAny #{type GdkEventType} (ForeignPtr GdkEventAny) deriving Show

pattern GdkEventGdkMap :: GdkEventAny -> GdkEvent
pattern GdkEventGdkMap p <- GdkEvent (GdkEventType #const GDK_MAP) (GdkEventAny #{const GDK_MAP} . castForeignPtr -> p)

pattern GdkEventGdkUnmap :: GdkEventAny -> GdkEvent
pattern GdkEventGdkUnmap p <- GdkEvent (GdkEventType #const GDK_UNMAP) (GdkEventAny #{const GDK_UNMAP} . castForeignPtr -> p)

newtype GdkEventVisibility = GdkEventVisibility (ForeignPtr GdkEventVisibility) deriving Show

pattern GdkEventGdkVisibilityNotify :: GdkEventVisibility -> GdkEvent
pattern GdkEventGdkVisibilityNotify p <-
	GdkEvent (GdkEventType #const GDK_VISIBILITY_NOTIFY) (GdkEventVisibility . castForeignPtr -> p)

newtype GdkVisibilityState = GdkVisibilityState #{type GdkVisibilityState} deriving Show

#enum GdkVisibilityState, GdkVisibilityState, GDK_VISIBILITY_UNOBSCURED, \
	GDK_VISIBILITY_PARTIAL, GDK_VISIBILITY_FULLY_OBSCURED

gdkEventVisibilityWindow :: GdkEventVisibility -> IO GdkWindow
gdkEventVisibilityWindow (GdkEventVisibility p) =
--	GdkWindow <$> (c_g_object_ref =<< withForeignPtr p #peek GdkEventVisibility, window)
	GdkWindow <$> withForeignPtr p #peek GdkEventVisibility, window

foreign import ccall "g_object_ref" c_g_object_ref :: Ptr a -> IO (Ptr a)

gdkEventVisibilityState :: GdkEventVisibility -> IO GdkVisibilityState
gdkEventVisibilityState (GdkEventVisibility p) = GdkVisibilityState <$> withForeignPtr p #peek GdkEventVisibility, state

pattern GdkEventGdkDelete :: GdkEventAny -> GdkEvent
pattern GdkEventGdkDelete p <- GdkEvent (GdkEventType #const GDK_DELETE) (GdkEventAny #{const GDK_DELETE} . castForeignPtr -> p)

pattern GdkEventGdkNothing :: GdkEventAny -> GdkEvent
pattern GdkEventGdkNothing p <- GdkEvent (GdkEventType (#const GDK_NOTHING)) (GdkEventAny (#const GDK_NOTHING) . castForeignPtr -> p)
-- pattern GdkEventGdkNothing p <- GdkEvent (GdkEventType (#const GDK_NOTHING)) (GdkEventAny . castForeignPtr -> p)

newtype GdkEventKey = GdkEventKey (ForeignPtr GdkEventKey) deriving Show

pattern GdkEventGdkKeyPress :: GdkEventKey -> GdkEvent
pattern GdkEventGdkKeyPress p <- GdkEvent (GdkEventType #const GDK_KEY_PRESS) (GdkEventKey . castForeignPtr -> p)

gdkEventKeyKeyval :: GdkEventKey -> IO #type guint
gdkEventKeyKeyval (GdkEventKey p) = withForeignPtr p #peek GdkEventKey, keyval

gdkEventKeyWindow :: GdkEventKey -> IO GdkWindow
gdkEventKeyWindow (GdkEventKey p) =
--	GdkWindow <$> (c_g_object_ref =<< withForeignPtr p #peek GdkEventVisibility, window)
	GdkWindow <$> withForeignPtr p #peek GdkEventVisibility, window

pattern GdkEventGdkKeyRelease :: GdkEventKey -> GdkEvent
pattern GdkEventGdkKeyRelease p <- GdkEvent (GdkEventType #const GDK_KEY_RELEASE) (GdkEventKey . castForeignPtr -> p)

newtype GdkEventFocus = GdkEventFocus (ForeignPtr GdkEventFocus) deriving Show

pattern GdkEventGdkFocusChange :: GdkEventFocus -> GdkEvent
pattern GdkEventGdkFocusChange p <-
	GdkEvent (GdkEventType #const GDK_FOCUS_CHANGE) (GdkEventFocus . castForeignPtr -> p)

gint16ToBool :: HasCallStack => #{type gint16} -> Bool
gint16ToBool #{const TRUE} = True
gint16ToBool #{const FALSE} = False
gint16ToBool _ = error "something wrong"

gdkEventFocusIn :: GdkEventFocus -> IO Bool
gdkEventFocusIn (GdkEventFocus p) = gint16ToBool <$> withForeignPtr p #peek GdkEventFocus, in
