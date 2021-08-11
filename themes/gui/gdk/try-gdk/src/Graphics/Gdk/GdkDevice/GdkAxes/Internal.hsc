{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.GdkAxes.Internal (
	-- * GDK AXES
	GdkAxes(..),

	-- * DEVICE
	gdkDeviceSetAxisUse,
	gdkDeviceGetAxisUse,

	gdkDeviceGetNAxes,
	gdkDeviceGetAxes,

	gdkDeviceGetAxis,
	gdkDeviceListAxes,
	gdkDeviceGetAxisValue,

	-- * COPY
	gdkAxesCopyFromPtr,

	-- * GDK AXIS USE
	GdkAxisUse(..),
	pattern GdkAxisIgnore,
	pattern GdkAxisX, pattern GdkAxisY,
	pattern GdkAxisPressure,
	pattern GdkAxisXtilt, pattern GdkAxisYtilt,
	pattern GdkAxisWheel, pattern GdkAxisDistance,
	pattern GdkAxisRotation, pattern GdkAxisSlider,
	pattern GdkAxisLast,

	-- * GDK AXIS FLAGS
	-- ** Multiple Flags
	GdkAxisFlags(..), gdkAxisFlags,
	pattern GdkAxisNoFlags,

	-- ** Single Flag
	GdkAxisFlag(..),
	gdkAxisFlagList,
	pattern GdkAxisFlagX, pattern GdkAxisFlagY,
	pattern GdkAxisFlagPressure,
	pattern GdkAxisFlagXtilt, pattern GdkAxisFlagYtilt,
	pattern GdkAxisFlagWheel, pattern GdkAxisFlagDistance,
	pattern GdkAxisFlagRotation, pattern GdkAxisFlagSlider

	) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int
import System.GLib.DoublyLinkedLists

import Graphics.Gdk.GdkDevice.Internal
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

#include <gdk/gdk.h>

newtype GdkAxes = GdkAxes (ForeignPtr CDouble) deriving Show

enum "GdkAxisUse" ''#{type GdkAxisUse} [''Show] [
	("GdkAxisIgnore", #{const GDK_AXIS_IGNORE}),
	("GdkAxisX", #{const GDK_AXIS_X}), ("GdkAxisY", #{const GDK_AXIS_Y}),
	("GdkAxisPressure", #{const GDK_AXIS_PRESSURE}),
	("GdkAxisXtilt", #{const GDK_AXIS_XTILT}),
	("GdkAxisYtilt", #{const GDK_AXIS_YTILT}),
	("GdkAxisWheel", #{const GDK_AXIS_WHEEL}),
	("GdkAxisDistance", #{const GDK_AXIS_DISTANCE}),
	("GdkAxisRotation", #{const GDK_AXIS_ROTATION}),
	("GdkAxisSlider", #{const GDK_AXIS_SLIDER}),
	("GdkAxisLast", #{const GDK_AXIS_LAST}) ]

gdkDeviceSetAxisUse :: IsGdkDevice d => d 'Pointer -> CUInt -> GdkAxisUse -> IO ()
gdkDeviceSetAxisUse = c_gdk_device_set_axis_use . getGdkDevice

foreign import ccall "gdk_device_set_axis_use"
	c_gdk_device_set_axis_use :: GdkDevice -> CUInt -> GdkAxisUse -> IO ()

gdkDeviceGetAxisUse :: IsGdkDevice d => d 'Pointer -> CUInt -> IO GdkAxisUse
gdkDeviceGetAxisUse = c_gdk_device_get_axis_use . getGdkDevice

foreign import ccall "gdk_device_get_axis_use"
	c_gdk_device_get_axis_use :: GdkDevice -> CUInt -> IO GdkAxisUse

gdkDeviceGetNAxes :: IsGdkDevice d => d 'Pointer -> IO CInt
gdkDeviceGetNAxes = c_gdk_device_get_n_axes . getGdkDevice

foreign import ccall "gdk_device_get_n_axes"
	c_gdk_device_get_n_axes :: GdkDevice -> IO CInt

enum "GdkAxisFlag" ''#{type GdkAxisFlags} [''Show] [
	("GdkAxisFlagX", #{const GDK_AXIS_FLAG_X}),
	("GdkAxisFlagY", #{const GDK_AXIS_FLAG_Y}),
	("GdkAxisFlagPressure", #{const GDK_AXIS_FLAG_PRESSURE}),
	("GdkAxisFlagXtilt", #{const GDK_AXIS_FLAG_XTILT}),
	("GdkAxisFlagYtilt", #{const GDK_AXIS_FLAG_YTILT}),
	("GdkAxisFlagWheel", #{const GDK_AXIS_FLAG_WHEEL}),
	("GdkAxisFlagDistance", #{const GDK_AXIS_FLAG_DISTANCE}),
	("GdkAxisFlagRotation", #{const GDK_AXIS_FLAG_ROTATION}),
	("GdkAxisFlagSlider", #{const GDK_AXIS_FLAG_SLIDER}) ]

enum "GdkAxisFlags" ''#{type GdkAxisFlags} [''Show] [ ("GdkAxisNoFlags", 0) ]

gdkAxisFlags :: [GdkAxisFlag] -> GdkAxisFlags
gdkAxisFlags = GdkAxisFlags . foldr ((.|.) . \(GdkAxisFlag af) -> af) 0

gdkAxisFlagList :: GdkAxisFlags -> [GdkAxisFlag]
gdkAxisFlagList (GdkAxisFlags afs) =
	GdkAxisFlag <$> separateBits (#{size GdkAxisFlags} * 8) afs

gdkDeviceGetAxes :: IsGdkDevice d => d 'Pointer -> IO GdkAxisFlags
gdkDeviceGetAxes = c_gdk_device_get_axes . getGdkDevice

foreign import ccall "gdk_device_get_axes"
	c_gdk_device_get_axes :: GdkDevice -> IO GdkAxisFlags

gdkDeviceGetAxis :: IsGdkDevice d => d 'Pointer -> GdkAxes -> GdkAxisUse -> IO (Maybe CDouble)
gdkDeviceGetAxis d (GdkAxes fas) au = withForeignPtr fas \pas -> alloca \v -> do
	b <- c_gdk_device_get_axis (getGdkDevice d) pas au v
	case b of
		#{const FALSE} -> pure Nothing
		#{const TRUE} -> Just <$> peek v
		_ -> error "gdk_device_get_axis should return FALSE or TRUE"

foreign import ccall "gdk_device_get_axis"
	c_gdk_device_get_axis ::
		GdkDevice -> Ptr CDouble -> GdkAxisUse -> Ptr CDouble ->
		IO #{type gboolean}

gdkDeviceListAxes :: IsGdkDevice d => d 'Pointer -> IO [GdkAtom]
gdkDeviceListAxes d =
	(maybe [] (GdkAtom <$>)) <$> (g_list_to_list =<< c_gdk_device_list_axes (getGdkDevice d))

foreign import ccall "gdk_device_list_axes"
	c_gdk_device_list_axes :: GdkDevice -> IO (Ptr (GList GdkAtom))

gdkDeviceGetAxisValue :: IsGdkDevice d => d 'Pointer -> GdkAxes -> GdkAtom -> IO (Maybe CDouble)
gdkDeviceGetAxisValue d (GdkAxes fas) lb =
	withForeignPtr fas \pas -> alloca \v ->
		c_gdk_device_get_axis_value (getGdkDevice d) pas lb v >>= \case
			#{const FALSE} -> pure Nothing
			#{const TRUE} -> Just <$> peek v
			_ -> error $ "gdk_device_get_axis_value" ++
				" should return FALSE or TRUE"

foreign import ccall "gdk_device_get_axis_value"
	c_gdk_device_get_axis_value ::
		GdkDevice -> Ptr CDouble -> GdkAtom -> Ptr CDouble ->
		IO #{type gboolean}

gdkAxesCopyFromPtr :: GdkDevice -> Ptr CDouble -> IO GdkAxes
gdkAxesCopyFromPtr d a = GdkAxes <$> do
	n <- c_gdk_device_get_n_axes d
	p <- mallocArray $ fromIntegral n
	copyArray p a $ fromIntegral n
	newForeignPtr p $ free p
