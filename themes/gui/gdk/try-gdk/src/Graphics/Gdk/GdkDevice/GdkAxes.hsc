{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice.GdkAxes where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.Enum
import Data.Bits
import Data.Bits.Misc
import Data.Word
import System.GLib.DoublyLinkedLists

import Graphics.Gdk.GdkDevice
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

#include <gdk/gdk.h>

newtype Axes = Axes (ForeignPtr CDouble) deriving Show

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

gdkDeviceListAxes :: GdkDevice -> IO [GdkAtom]
gdkDeviceListAxes d =
	map GdkAtom <$> (g_list_to_list =<< c_gdk_device_list_axes d)

foreign import ccall "gdk_device_list_axes"
	c_gdk_device_list_axes :: GdkDevice -> IO (Ptr (GList GdkAtom))
