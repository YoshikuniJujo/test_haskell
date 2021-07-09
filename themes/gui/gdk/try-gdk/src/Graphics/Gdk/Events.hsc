{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Events (
	-- * CHECKED

	-- * USE
	GdkEventMaskMultiBits(..), getGdkEventMask, gdkEventMaskMultiBits,
	pattern GdkExposureMask, pattern GdkPointerMotionMask,
	pattern GdkButtonMotionMask, pattern GdkButton1MotionMask,
	pattern GdkButton2MotionMask, pattern GdkButton3MotionMask,
	pattern GdkButtonPressMask, pattern GdkButtonReleaseMask,
	pattern GdkKeyPressMask, pattern GdkKeyReleaseMask,

	gdkEventGet,

	GdkEventMotion,
	pattern GdkEventGdkDelete, pattern GdkEventGdkMotionNotify,
	pattern GdkEventGdkKeyPress,
	pattern GdkEventGdkUnmap, pattern GdkEventGdkConfigure,
	pattern GdkEventGdkWindowState,
	pattern GdkEventGdkFocusChange,
	pattern GdkEventGdkNothing, pattern GdkEventGdkKeyRelease,
	pattern GdkEventGdkMap,

	gdkEventMotionX, gdkEventMotionY, gdkEventGetDeviceTool,
	gdkEventGetSourceDevice,

	gdkEventConfigureHeight,
	gdkEventWindowStateNewWindowState,
	gdkEventConfigureX, gdkEventConfigureY, gdkEventConfigureWidth,
	gdkEventFocusIn, pattern GdkZeroEventsMask, pattern GdkFocusChangeMask,

	gdkGetShowEvents, pattern GdkEnterNotifyMask, pattern GdkLeaveNotifyMask,
	gdkEventMaskSingleBitList, gdkEventFocusWindow, gdkEventConfigureWindow,

	-- * NOT USE
	gdkEventsPending, gdkEventPeek, gdkEventPut, gdkEventNew, gdkEventCopy, gdkEventGetAxis,
	gdkEventGetButton, gdkEventGetClickCount, gdkEventGetCoords, gdkEventGetKeycode, gdkEventGetKeyval,
	gdkEventGetRootCoords, gdkEventGetScrollDirection, gdkEventGetScrollDeltas, gdkEventIsScrollStopEvent,
	gdkEventGetState, gdkEventGetTime, gdkEventGetWindow, gdkEventGetEventType, gdkEventGetSeat,
	gdkEventGetScancode, gdkEventSetScreen, gdkEventGetScreen, gdkEventGetDevice, gdkEventSetDevice,
	gdkEventSetSourceDevice,

	) where

import GHC.Stack
import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Bool
import Data.Word
import Data.Int
import System.GLib.Bool

import {-# SOURCE #-} Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkDevice
import {-# SOURCE #-} Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Graphics.Gdk.Values
import {-# SOURCE #-} Graphics.Gdk.GdkSeat

#include <gdk/gdk.h>

foreign import ccall "gdk_events_pending" c_gdk_events_pending :: IO #type gboolean

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

foreign import ccall "gdk_event_get_scancode" c_gdk_event_get_scancode ::
	Ptr GdkEvent -> IO #type int

gdkEventGetScancode :: GdkEvent -> IO #type int
gdkEventGetScancode (GdkEvent _ fe) = withForeignPtr fe c_gdk_event_get_scancode

foreign import ccall "gdk_get_show_events" c_gdk_get_show_events ::
	IO #type gboolean

gdkGetShowEvents :: IO Bool
gdkGetShowEvents = gbooleanToBool <$> c_gdk_get_show_events

foreign import ccall "gdk_event_set_screen" c_gdk_event_set_screen ::
	Ptr GdkEvent -> Ptr GdkScreen -> IO ()

gdkEventSetScreen :: GdkEvent -> GdkScreen -> IO ()
gdkEventSetScreen (GdkEvent _ fe) (GdkScreen s) = withForeignPtr fe \e ->
	c_gdk_event_set_screen e s

foreign import ccall "gdk_event_get_screen" c_gdk_event_get_screen ::
	Ptr GdkEvent -> IO (Ptr GdkScreen)

gdkEventGetScreen :: GdkEvent -> IO GdkScreen
gdkEventGetScreen (GdkEvent _ fe) = withForeignPtr fe \e ->
	GdkScreen <$> c_gdk_event_get_screen e

foreign import ccall "gdk_event_get_device" c_gdk_event_get_device ::
	Ptr GdkEvent -> IO (Ptr GdkDevice)

gdkEventGetDevice :: GdkEvent -> IO GdkDevice
gdkEventGetDevice (GdkEvent _ fe) =
	withForeignPtr fe \e -> GdkDevice <$> c_gdk_event_get_device e

foreign import ccall "gdk_event_set_device" c_gdk_event_set_device ::
	Ptr GdkEvent -> Ptr GdkDevice -> IO ()

gdkEventSetDevice :: GdkEvent -> GdkDevice -> IO ()
gdkEventSetDevice (GdkEvent _ fe) (GdkDevice d) =
	withForeignPtr fe \e -> c_gdk_event_set_device e d

foreign import ccall "gdk_event_get_source_device" c_gdk_event_get_source_device ::
	Ptr GdkEvent -> IO (Ptr GdkDevice)

gdkEventGetSourceDevice :: GdkEvent -> IO (Maybe GdkDevice)
gdkEventGetSourceDevice (GdkEvent _ fe) = withForeignPtr fe \e ->
	(<$> c_gdk_event_get_source_device e) \case
		NullPtr -> Nothing; p -> Just $ GdkDevice p

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

pattern GdkEventGdkMap :: GdkEventAny -> GdkEvent
pattern GdkEventGdkMap p <- GdkEvent t@(GdkEventType #const GDK_MAP) (GdkEventAny t . castForeignPtr -> p)

pattern GdkEventGdkUnmap :: GdkEventAny -> GdkEvent
pattern GdkEventGdkUnmap p <- GdkEvent t@(GdkEventType #const GDK_UNMAP) (GdkEventAny t . castForeignPtr -> p)

gdkEventConfigureWindow :: GdkEventConfigure -> IO GdkWindow
gdkEventConfigureWindow (GdkEventConfigure p) = GdkWindow <$> withForeignPtr p #peek GdkEventConfigure, window

gdkEventFocusWindow :: GdkEventFocus -> IO GdkWindow
gdkEventFocusWindow (GdkEventFocus p) =
	GdkWindow <$> withForeignPtr p #peek GdkEventFocus, window

pattern GdkEventGdkDelete :: GdkEventAny -> GdkEvent
pattern GdkEventGdkDelete p <- GdkEvent t@(GdkEventType #const GDK_DELETE) (GdkEventAny t . castForeignPtr -> p)

pattern GdkEventGdkNothing :: GdkEventAny -> GdkEvent
pattern GdkEventGdkNothing p <- GdkEvent t@(GdkEventType (#const GDK_NOTHING)) (GdkEventAny t . castForeignPtr -> p)
-- pattern GdkEventGdkNothing p <- GdkEvent (GdkEventType (#const GDK_NOTHING)) (GdkEventAny . castForeignPtr -> p)

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

newtype GdkEventMotion = GdkEventMotion (ForeignPtr GdkEventMotion) deriving Show

pattern GdkEventGdkMotionNotify :: GdkEventMotion -> GdkEvent
pattern GdkEventGdkMotionNotify p <- GdkEvent (GdkEventType #const GDK_MOTION_NOTIFY) (GdkEventMotion . castForeignPtr -> p)

gdkEventMotionX, gdkEventMotionY :: GdkEventMotion -> IO #type gdouble
gdkEventMotionX (GdkEventMotion fm) = withForeignPtr fm #peek GdkEventMotion, x
gdkEventMotionY (GdkEventMotion fm) = withForeignPtr fm #peek GdkEventMotion, y

foreign import ccall "gdk_event_set_source_device" c_gdk_event_set_source_device ::
	Ptr GdkEvent -> Ptr GdkDevice -> IO ()

gdkEventSetSourceDevice :: GdkEvent -> GdkDevice -> IO ()
gdkEventSetSourceDevice (GdkEvent _ fe) (GdkDevice pd) =
	withForeignPtr fe \e -> c_gdk_event_set_source_device e pd

foreign import ccall "gdk_event_get_device_tool" c_gdk_event_get_device_tool ::
	Ptr GdkEvent -> IO (Ptr GdkDeviceTool)

gdkEventGetDeviceTool :: GdkEvent -> IO (Maybe GdkDeviceTool)
gdkEventGetDeviceTool (GdkEvent _ fe) = withForeignPtr fe \e -> do
	(<$> c_gdk_event_get_device_tool e) \case
		NullPtr -> Nothing; p -> Just $ GdkDeviceTool p

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

gdkEventMaskSingleBitList :: GdkEventMaskMultiBits -> [GdkEventMaskSingleBit]
gdkEventMaskSingleBitList (GdkEventMaskMultiBits ems) =
	GdkEventMaskSingleBit <$> separateBits (#{size GdkEventMask} * 8) ems

mergeGdkEventMask :: [GdkEventMaskSingleBit] -> #{type GdkEventMask}
mergeGdkEventMask [] = 0
mergeGdkEventMask (GdkEventMaskSingleBit em : ems) = em .|. mergeGdkEventMask ems
