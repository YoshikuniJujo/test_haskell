{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkSeat where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Control.Arrow
import Data.Word
import Data.Int

import Graphics.Gdk.Events
import Graphics.Gdk.Types
import Graphics.Gdk.Values

import System.GLib.DoublyLinkedLists

#include <gdk/gdk.h>

foreign import ccall "gdk_seat_get_display" c_gdk_seat_get_display ::
	Ptr GdkSeat -> IO (Ptr GdkDisplay)

gdkSeatGetDisplay :: GdkSeat -> IO GdkDisplay
gdkSeatGetDisplay (GdkSeat p) = GdkDisplay <$> c_gdk_seat_get_display p

foreign import ccall "gdk_seat_get_pointer" c_gdk_seat_get_pointer ::
	Ptr GdkSeat -> IO (Ptr GdkDevice)

gdkSeatGetPointer :: GdkSeat -> IO GdkDevice
gdkSeatGetPointer (GdkSeat p) = GdkDevice
	<$> (flip newForeignPtr (pure ()) =<< c_gdk_seat_get_pointer p)

foreign import ccall "gdk_seat_get_keyboard" c_gdk_seat_get_keyboard ::
	Ptr GdkSeat -> IO (Ptr GdkDevice)

gdkSeatGetKeyboard :: GdkSeat -> IO GdkDevice
gdkSeatGetKeyboard (GdkSeat p) = GdkDevice <$> (flip newForeignPtr (pure ()) =<< c_gdk_seat_get_keyboard p)

foreign import ccall "gdk_seat_get_capabilities" c_gdk_seat_get_capabilities ::
	Ptr GdkSeat -> IO #type GdkSeatCapabilities

gdkSeatGetCapabilities :: GdkSeat -> IO GdkSeatCapabilities
gdkSeatGetCapabilities (GdkSeat p) = GdkSeatCapabilities <$> c_gdk_seat_get_capabilities p

foreign import ccall "gdk_seat_get_slaves" c_gdk_seat_get_slaves ::
	Ptr GdkSeat -> #{type GdkSeatCapabilities} -> IO (Ptr (GList GdkDevice))

mkGdkDevice :: Ptr GdkDevice -> IO GdkDevice
mkGdkDevice p = GdkDevice <$> newForeignPtr p (pure ())

gdkSeatGetSlaves :: GdkSeat -> GdkSeatCapabilities -> IO ([GdkDevice], [GdkDevice])
gdkSeatGetSlaves (GdkSeat p) (GdkSeatCapabilities cps) = do
	gl <- c_gdk_seat_get_slaves p cps
	(\(x, y) -> (,) <$> mapM mkGdkDevice x <*> mapM mkGdkDevice y) =<< gListListPtr (GListRef gl)
		<* c_g_list_free gl

foreign import ccall "g_list_free" c_g_list_free :: Ptr (GList a) -> IO ()

foreign import ccall "gdk_seat_grab" c_gdk_seat_grab ::
	Ptr GdkSeat -> Ptr GdkWindow -> #{type GdkSeatCapabilities} -> #{type gboolean} ->
	Ptr GdkCursor -> Ptr GdkEvent ->
	FunPtr (C_GdkSeatGrabPrepareFunc a) -> Ptr a -> IO #{type GdkGrabStatus}

type C_GdkSeatGrabPrepareFunc a = Ptr GdkSeat -> Ptr GdkWindow -> Ptr a -> IO ()

class Pointerable a where toPtr :: a -> IO (Ptr a); fromPtr :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => Pointerable a where
	toPtr x = alloca \p -> p <$ poke p x
	fromPtr = peek

type GdkSeatGrabPrepareFunc a = GdkSeat -> GdkWindow -> a -> IO ()

convertGdkSeatGrabPrepareFunc ::
	Pointerable a => GdkSeatGrabPrepareFunc a -> C_GdkSeatGrabPrepareFunc a
convertGdkSeatGrabPrepareFunc f st wn x = f (GdkSeat st) (GdkWindow wn) =<< fromPtr x

foreign import ccall "wrapper" wrap_GdkSeatGrabPrepareFunc :: C_GdkSeatGrabPrepareFunc a -> IO (FunPtr (C_GdkSeatGrabPrepareFunc a))

gdkSeatGrabSimple :: GdkSeat -> GdkWindow -> IO GdkGrabStatus
gdkSeatGrabSimple st wn =
-- gdkSeatGrabSimple (GdkSeat st) (GdkWindow wn) =
	gdkSeatGrab st wn gdkSeatCapabilityAllPointing False Nothing Nothing (Nothing :: Maybe (GdkSeatGrabPrepareFunc (), ()))
--	c_gdk_seat_grab st wn #{const GDK_SEAT_CAPABILITY_ALL_POINTING} #{const TRUE} nullPtr nullPtr nullPtr nullPtr
--	c_gdk_seat_grab st wn #{const GDK_SEAT_CAPABILITY_ALL_POINTING} #{const FALSE} nullPtr nullPtr nullFunPtr nullPtr

withGdkCursor :: Maybe GdkCursor -> (Ptr GdkCursor -> IO a) -> IO a
withGdkCursor mc f = case mc of
	Nothing -> f nullPtr
	Just (GdkCursor fc) -> withForeignPtr fc f

withGdkEvent :: Maybe GdkEvent -> (Ptr GdkEvent -> IO a) -> IO a
withGdkEvent me f = case me of
	Nothing -> f nullPtr
	Just (GdkEvent _ fev) -> withForeignPtr fev f

maybeGdkSeatGrabPrepareFunc ::
	Pointerable a =>
	Maybe (GdkSeatGrabPrepareFunc a, a) -> IO (FunPtr (C_GdkSeatGrabPrepareFunc a), Ptr a)
maybeGdkSeatGrabPrepareFunc = \case
	Nothing -> pure (nullFunPtr, nullPtr)
	Just (f, x) -> do
		fp <- wrap_GdkSeatGrabPrepareFunc $ convertGdkSeatGrabPrepareFunc f
		px <- toPtr x
		pure (fp, px)

gdkSeatGrab ::
	Pointerable a =>
	GdkSeat -> GdkWindow -> GdkSeatCapabilities -> Bool -> Maybe GdkCursor ->
	Maybe GdkEvent -> Maybe (GdkSeatGrabPrepareFunc a, a) -> IO GdkGrabStatus
gdkSeatGrab (GdkSeat st) (GdkWindow wn) (GdkSeatCapabilities cp) oe
	crs ev fx = withGdkCursor crs \pcrs -> withGdkEvent ev \pev -> do
	(fp, px) <- maybeGdkSeatGrabPrepareFunc fx
	GdkGrabStatus <$> c_gdk_seat_grab st wn cp (boolToGboolean oe) pcrs pev fp px

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE
