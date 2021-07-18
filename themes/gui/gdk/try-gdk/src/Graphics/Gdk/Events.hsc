{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Events (
	-- * CHECKED
	gdkEventsPending, gdkWithEventPeek, gdkWithEventGet, gdkEventPut,
	gdkWithEventNew, gdkWithEventCopy,

	-- * USE
	GdkEventMaskMultiBits(..), getGdkEventMask, gdkEventMaskMultiBits,
	pattern GdkExposureMask, pattern GdkPointerMotionMask,
	pattern GdkButtonMotionMask, pattern GdkButton1MotionMask,
	pattern GdkButton2MotionMask, pattern GdkButton3MotionMask,
	pattern GdkButtonPressMask, pattern GdkButtonReleaseMask,
	pattern GdkKeyPressMask, pattern GdkKeyReleaseMask,
	pattern GdkScrollMask, pattern GdkSmoothScrollMask,

	gdkEventGetDeviceTool,
	gdkEventSealedGetDeviceTool,
	gdkEventGetSourceDevice,
	gdkEventSealedGetSourceDevice,

	gdkEventConfigureHeight,
	gdkEventConfigureX, gdkEventConfigureY, gdkEventConfigureWidth,
	pattern GdkZeroEventsMask, pattern GdkFocusChangeMask,

	gdkGetShowEvents, pattern GdkEnterNotifyMask, pattern GdkLeaveNotifyMask,
	gdkEventMaskSingleBitList, gdkEventConfigureWindow,

	-- * NOT USE
	gdkEventGetScrollDeltas, gdkEventIsScrollStopEvent,
	gdkEventGetState, gdkEventGetTime, gdkEventGetWindow, gdkEventGetEventType, gdkEventGetSeat,
	gdkEventGetScancode, gdkEventSetScreen, gdkEventGetScreen, gdkEventGetDevice, gdkEventSetDevice,
	gdkEventSetSourceDevice,

	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
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

gdkEventsPending :: IO Bool
gdkEventsPending = gbooleanToBool <$> c_gdk_events_pending

foreign import ccall "gdk_events_pending"
	c_gdk_events_pending :: IO #type gboolean

gdkWithEventPeek :: (forall s . Maybe (GdkEventSealed s) -> IO a) -> IO a
gdkWithEventPeek f = c_gdk_event_peek >>= \case
	NullPtr -> f Nothing
	p -> (f . Just . GdkEventSealed =<< newForeignPtr p (pure ()))
		<* c_gdk_event_free p

foreign import ccall "gdk_event_peek" c_gdk_event_peek :: IO (Ptr GdkEvent)

gdkWithEventGet :: (forall s . Maybe (GdkEventSealed s) -> IO a) -> IO a
gdkWithEventGet f = c_gdk_event_get >>= \case
	NullPtr -> f Nothing
	p -> (f . Just . GdkEventSealed =<< newForeignPtr p (pure ()))
		<* c_gdk_event_free p

foreign import ccall "gdk_event_get" c_gdk_event_get :: IO (Ptr GdkEvent)

gdkEventPut :: GdkEventSealed s -> IO ()
gdkEventPut (GdkEventSealed fe) = withForeignPtr fe c_gdk_event_put

foreign import ccall "gdk_event_put" c_gdk_event_put :: Ptr GdkEvent -> IO ()

gdkWithEventNew :: (forall s . GdkEventSealed s -> IO a) -> IO a
gdkWithEventNew f = c_gdk_event_new >>= \case
	NullPtr -> error "gdk_event_new cannot return NULL"
	p -> (f . GdkEventSealed =<< newForeignPtr p (pure ()))
		<* c_gdk_event_free p

foreign import ccall "gdk_event_new" c_gdk_event_new :: IO (Ptr GdkEvent)

gdkWithEventCopy ::
	GdkEventSealed s -> (forall t . GdkEventSealed t -> IO a) -> IO a
gdkWithEventCopy (GdkEventSealed fe) f = withForeignPtr fe c_gdk_event_copy >>= \case
	NullPtr -> error "gdk_event_copy cannot return NULL"
	p -> (f . GdkEventSealed =<< newForeignPtr p (pure ()))
		<* c_gdk_event_free p

foreign import ccall "gdk_event_copy" c_gdk_event_copy :: Ptr GdkEvent -> IO (Ptr GdkEvent)

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
	Ptr GdkEvent -> IO CInt

gdkEventGetScancode :: GdkEventSealed s -> IO CInt
gdkEventGetScancode (GdkEventSealed fe) = withForeignPtr fe c_gdk_event_get_scancode

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

gdkEventSealedGetSourceDevice :: GdkEventSealed s -> IO (Maybe GdkDevice)
gdkEventSealedGetSourceDevice (GdkEventSealed fe) = withForeignPtr fe \e ->
	(<$> c_gdk_event_get_source_device e) \case
		NullPtr -> Nothing; p -> Just $ GdkDevice p

gdkEventGetSourceDevice :: GdkEvent -> IO (Maybe GdkDevice)
gdkEventGetSourceDevice (GdkEvent _ fe) = withForeignPtr fe \e ->
	(<$> c_gdk_event_get_source_device e) \case
		NullPtr -> Nothing; p -> Just $ GdkDevice p

foreign import ccall "gdk_event_set_source_device" c_gdk_event_set_source_device ::
	Ptr GdkEvent -> Ptr GdkDevice -> IO ()

gdkEventSetSourceDevice :: GdkEvent -> GdkDevice -> IO ()
gdkEventSetSourceDevice (GdkEvent _ fe) (GdkDevice pd) =
	withForeignPtr fe \e -> c_gdk_event_set_source_device e pd

foreign import ccall "gdk_event_get_device_tool" c_gdk_event_get_device_tool ::
	Ptr GdkEvent -> IO (Ptr GdkDeviceTool)

gdkEventSealedGetDeviceTool :: GdkEventSealed s -> IO (Maybe GdkDeviceTool)
gdkEventSealedGetDeviceTool (GdkEventSealed fe) = withForeignPtr fe \e -> do
	(<$> c_gdk_event_get_device_tool e) \case
		NullPtr -> Nothing; p -> Just $ GdkDeviceTool p

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
	("GdkScrollMask", #{const GDK_SCROLL_MASK}),
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
