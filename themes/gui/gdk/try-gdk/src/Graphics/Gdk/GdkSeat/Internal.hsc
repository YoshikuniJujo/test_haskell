{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkSeat.Internal (
	-- * GDK SEAT
	GdkSeat(..),

	-- * GET

	gdkSeatGetDisplay,
	gdkSeatGetCapabilities,
	gdkSeatGetPointer, gdkSeatGetKeyboard,
	gdkSeatGetSlaves,

	-- * GRAB

	gdkSeatGrab, gdkSeatGrabSimple, gdkSeatUngrab, GdkSeatGrabPrepareFunc,

	-- * GDK SEAT CAPABILITIES

	GdkSeatCapabilities(..),
	pattern GdkSeatCapabilityNone,
	pattern GdkSeatCapabilityAllPointing,
	pattern GdkSeatCapabilityAll,

	gdkSeatCapabilities,

	GdkSeatCapability(..),
	pattern GdkSeatCapabilityPointer,
	pattern GdkSeatCapabilityTouch,
	pattern GdkSeatCapabilityTabletStylus,
	pattern GdkSeatCapabilityKeyboard,

	gdkSeatCapabilityList,

	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int

import Graphics.Gdk.General
import {-# SOURCE #-} Graphics.Gdk.GdkDisplay.Internal
import Graphics.Gdk.GdkDevice.Internal
import Graphics.Gdk.Cursors.Internal
import {-# SOURCE #-} Graphics.Gdk.Windows.Internal
import Graphics.Gdk.EventStructures.Internal

import System.GLib.DoublyLinkedLists
import System.GLib.Pointerable
import System.GLib.Bool

#include <gdk/gdk.h>

newtype GdkSeat = GdkSeat (Ptr GdkSeat) deriving Show

enum "GdkSeatCapability" ''#{type GdkSeatCapabilities} [''Show] [
	("GdkSeatCapabilityPointer", #{const GDK_SEAT_CAPABILITY_POINTER}),
	("GdkSeatCapabilityTouch", #{const GDK_SEAT_CAPABILITY_TOUCH}),
	("GdkSeatCapabilityTabletStylus", #{const GDK_SEAT_CAPABILITY_TABLET_STYLUS}),
	("GdkSeatCapabilityKeyboard", #{const GDK_SEAT_CAPABILITY_KEYBOARD}) ]

enum "GdkSeatCapabilities" ''#{type GdkSeatCapabilities} [''Show] [
	("GdkSeatCapabilityNone", #{const GDK_SEAT_CAPABILITY_NONE}),
	("GdkSeatCapabilityAllPointing", #{const GDK_SEAT_CAPABILITY_ALL_POINTING}),
	("GdkSeatCapabilityAll", #{const GDK_SEAT_CAPABILITY_ALL}) ]

consGdkSeatCapability ::
	GdkSeatCapability -> GdkSeatCapabilities -> GdkSeatCapabilities
consGdkSeatCapability (GdkSeatCapability c) (GdkSeatCapabilities cs) =
	GdkSeatCapabilities $ c .|. cs

gdkSeatCapabilities :: [GdkSeatCapability] -> GdkSeatCapabilities
gdkSeatCapabilities = foldr consGdkSeatCapability GdkSeatCapabilityNone

gdkSeatCapabilityList :: GdkSeatCapabilities -> [GdkSeatCapability]
gdkSeatCapabilityList (GdkSeatCapabilities cs) = map GdkSeatCapability $ separateBits 32 cs

foreign import ccall "gdk_seat_get_display" c_gdk_seat_get_display ::
	GdkSeat -> IO (Ptr GdkDisplay)

gdkSeatGetDisplay :: GdkSeat -> IO GdkDisplay
gdkSeatGetDisplay p = GdkDisplay <$> c_gdk_seat_get_display p

foreign import ccall "gdk_seat_get_pointer" c_gdk_seat_get_pointer ::
	GdkSeat -> IO (Ptr GdkDevice)

gdkSeatGetPointer :: GdkSeat -> IO (GdkDeviceMaster 'Pointer)
gdkSeatGetPointer p = GdkDeviceMaster <$> c_gdk_seat_get_pointer p

foreign import ccall "gdk_seat_get_keyboard" c_gdk_seat_get_keyboard ::
	GdkSeat -> IO (Ptr GdkDevice)

gdkSeatGetKeyboard :: GdkSeat -> IO (GdkDeviceMaster 'Keyboard)
gdkSeatGetKeyboard p = GdkDeviceMaster <$> c_gdk_seat_get_keyboard p

foreign import ccall "gdk_seat_get_capabilities" c_gdk_seat_get_capabilities ::
	GdkSeat -> IO #type GdkSeatCapabilities

gdkSeatGetCapabilities :: GdkSeat -> IO GdkSeatCapabilities
gdkSeatGetCapabilities p = GdkSeatCapabilities <$> c_gdk_seat_get_capabilities p

foreign import ccall "gdk_seat_get_slaves" c_gdk_seat_get_slaves ::
	GdkSeat -> #{type GdkSeatCapabilities} -> IO (Ptr (GList GdkDevice))

gdkSeatGetSlaves :: forall pk . PointerOrKeyboard pk => GdkSeat -> GdkSeatCapabilities -> IO [GdkDevicePhysical pk]
gdkSeatGetSlaves p (GdkSeatCapabilities cps) = do
	gl <- c_gdk_seat_get_slaves p cps
	maybe (pure []) ((map GdkDevicePhysical <$>) . filterPK @pk) =<< (g_list_to_list gl <* c_g_list_free gl)

gdkSeatGrab ::
	Pointerable a =>
	GdkSeat -> GdkWindow -> GdkSeatCapabilities -> Bool -> Maybe GdkCursor ->
	Maybe (GdkEvent s) -> Maybe (GdkSeatGrabPrepareFunc a, a) -> IO GdkGrabStatus
gdkSeatGrab st (GdkWindow wn) (GdkSeatCapabilities cp) oe
	crs ev fx = withGdkCursor crs \pcrs -> withGdkEvent ev \pev -> do
	withGdkSeatGrabPrepareFunc fx \fp px ->
		GdkGrabStatus <$> c_gdk_seat_grab st wn cp (boolToGboolean oe) pcrs pev fp px

foreign import ccall "gdk_seat_grab" c_gdk_seat_grab ::
	GdkSeat -> Ptr GdkWindow -> #{type GdkSeatCapabilities} -> #{type gboolean} ->
	Ptr GdkCursor -> Ptr GdkEventTag ->
	FunPtr (C_GdkSeatGrabPrepareFunc a) -> Ptr a -> IO #{type GdkGrabStatus}

type C_GdkSeatGrabPrepareFunc a = GdkSeat -> Ptr GdkWindow -> Ptr a -> IO ()

type GdkSeatGrabPrepareFunc a = GdkSeat -> GdkWindow -> a -> IO ()

convertGdkSeatGrabPrepareFunc ::
	Pointerable a => GdkSeatGrabPrepareFunc a -> C_GdkSeatGrabPrepareFunc a
convertGdkSeatGrabPrepareFunc f st wn x = f st (GdkWindow wn) =<< fromPtr x

foreign import ccall "wrapper" wrap_GdkSeatGrabPrepareFunc ::
	C_GdkSeatGrabPrepareFunc a -> IO (FunPtr (C_GdkSeatGrabPrepareFunc a))

gdkSeatGrabSimple :: GdkSeat -> GdkWindow -> IO GdkGrabStatus
gdkSeatGrabSimple st wn = gdkSeatGrab st wn GdkSeatCapabilityAllPointing
	False Nothing Nothing (Nothing :: Maybe (GdkSeatGrabPrepareFunc (), ()))

withGdkCursor :: Maybe GdkCursor -> (Ptr GdkCursor -> IO a) -> IO a
withGdkCursor mc f = case mc of
	Nothing -> f nullPtr
	Just (GdkCursor fc) -> withForeignPtr fc f

withGdkEvent :: Maybe (GdkEvent s) -> (Ptr GdkEventTag -> IO a) -> IO a
withGdkEvent me f = case me of
	Nothing -> f nullPtr
	Just (GdkEvent fev) -> withForeignPtr fev f

withGdkSeatGrabPrepareFunc ::
	Pointerable a =>
	Maybe (GdkSeatGrabPrepareFunc a, a) ->
	(FunPtr (C_GdkSeatGrabPrepareFunc a) -> Ptr a -> IO b) -> IO b
withGdkSeatGrabPrepareFunc xx ff = case xx of
	Nothing -> ff nullFunPtr nullPtr
	Just (f, x) -> do
		fp <- wrap_GdkSeatGrabPrepareFunc $ convertGdkSeatGrabPrepareFunc f
		withPtr x $ ff fp

foreign import ccall "gdk_seat_ungrab" gdkSeatUngrab :: GdkSeat -> IO ()
