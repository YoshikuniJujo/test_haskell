{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.GdkAxes where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Word
import Data.Int
import System.GLib.DoublyLinkedLists

import Graphics.Gdk.GdkDevice
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

#include <gdk/gdk.h>

newtype GdkAxes = GdkAxes (ForeignPtr CDouble) deriving Show

enum "GdkAxisUse" ''#{type GdkAxisUse} [''Show] [
	("GdkAxisIgnore", #{const GDK_AXIS_IGNORE}),
	("GdkAxisX", #{const GDK_AXIS_X}),
	("GdkAxisY", #{const GDK_AXIS_Y}),
	("GdkAxisPressure", #{const GDK_AXIS_PRESSURE}),
	("GdkAxisXtilt", #{const GDK_AXIS_XTILT}),
	("GdkAxisYtilt", #{const GDK_AXIS_YTILT}),
	("GdkAxisWheel", #{const GDK_AXIS_WHEEL}),
	("GdkAxisDistance", #{const GDK_AXIS_DISTANCE}),
	("GdkAxisRotation", #{const GDK_AXIS_ROTATION}),
	("GdkAxisSlider", #{const GDK_AXIS_SLIDER}),
	("GdkAxisLast", #{const GDK_AXIS_LAST}) ]

foreign import ccall "gdk_device_set_axis_use"
	gdkDeviceSetAxisUse :: GdkDevice -> CUInt -> GdkAxisUse -> IO ()

foreign import ccall "gdk_device_get_axis_use"
	gdkDeviceGetAxisUse :: GdkDevice -> CUInt -> IO GdkAxisUse

foreign import ccall "gdk_device_get_n_axes"
	gdkDeviceGetNAxes :: GdkDevice -> IO CInt

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

foreign import ccall "gdk_device_get_axes"
	gdkDeviceGetAxes :: GdkDevice -> IO GdkAxisFlags

gdkDeviceGetAxis :: GdkDevice -> GdkAxes -> GdkAxisUse -> IO (Maybe CDouble)
gdkDeviceGetAxis d (GdkAxes fas) au = withForeignPtr fas \pas -> alloca \v -> do
	b <- c_gdk_device_get_axis d pas au v
	case b of
		#{const FALSE} -> pure Nothing
		#{const TRUE} -> Just <$> peek v
		_ -> error "gdk_device_get_axis should return FALSE or TRUE"

foreign import ccall "gdk_device_get_axis"
	c_gdk_device_get_axis ::
		GdkDevice -> Ptr CDouble -> GdkAxisUse -> Ptr CDouble ->
		IO #{type gboolean}

gdkDeviceListAxes :: GdkDevice -> IO [GdkAtom]
gdkDeviceListAxes d =
	map GdkAtom <$> (g_list_to_list =<< c_gdk_device_list_axes d)

foreign import ccall "gdk_device_list_axes"
	c_gdk_device_list_axes :: GdkDevice -> IO (Ptr (GList GdkAtom))

gdkDeviceGetAxisValue :: GdkDevice -> GdkAxes -> GdkAtom -> IO (Maybe CDouble)
gdkDeviceGetAxisValue d (GdkAxes fas) lb =
	withForeignPtr fas \pas -> alloca \v ->
		c_gdk_device_get_axis_value d pas lb v >>= \case
			#{const FALSE} -> pure Nothing
			#{const TRUE} -> Just <$> peek v
			_ -> error $ "gdk_device_get_axis_value" ++
				" should return FALSE or TRUE"

foreign import ccall "gdk_device_get_axis_value"
	c_gdk_device_get_axis_value ::
		GdkDevice -> Ptr CDouble -> GdkAtom -> Ptr CDouble ->
		IO #{type gboolean}
