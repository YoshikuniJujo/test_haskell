{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Events (
	-- * CHECKED
	gdkEventsPending, gdkWithEventPeek, gdkWithEventGet, gdkEventPut,
	gdkWithEventNew, gdkWithEventCopy,
	gdkGetShowEvents, gdkSetShowEvents,

	-- * USE
	GdkEventMaskMultiBits(..), getGdkEventMask, gdkEventMaskMultiBits,
	pattern GdkExposureMask, pattern GdkPointerMotionMask,
	pattern GdkButtonMotionMask, pattern GdkButton1MotionMask,
	pattern GdkButton2MotionMask, pattern GdkButton3MotionMask,
	pattern GdkButtonPressMask, pattern GdkButtonReleaseMask,
	pattern GdkKeyPressMask, pattern GdkKeyReleaseMask,
	pattern GdkScrollMask, pattern GdkTouchMask,
	pattern GdkSmoothScrollMask,

	gdkEventConfigureHeight,
	gdkEventConfigureX, gdkEventConfigureY, gdkEventConfigureWidth,
	pattern GdkZeroEventsMask, pattern GdkFocusChangeMask,

	pattern GdkEnterNotifyMask, pattern GdkLeaveNotifyMask,
	gdkEventMaskSingleBitList, gdkEventConfigureWindow,

	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int
import System.GLib.Bool

import Graphics.Gdk.EventStructures

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

gdkGetShowEvents :: IO Bool
gdkGetShowEvents = gbooleanToBool <$> c_gdk_get_show_events

foreign import ccall "gdk_get_show_events"
	c_gdk_get_show_events :: IO #type gboolean

gdkSetShowEvents :: Bool -> IO ()
gdkSetShowEvents = c_gdk_set_show_events . boolToGboolean

foreign import ccall "gdk_set_show_events"
	c_gdk_set_show_events :: #{type gboolean} -> IO ()

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
