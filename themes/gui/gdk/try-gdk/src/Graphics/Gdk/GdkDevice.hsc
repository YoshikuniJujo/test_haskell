{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.GdkDevice where

import Foreign.Ptr
import Foreign.C
import Data.Word

import Graphics.Gdk.Types
import Graphics.Gdk.Values

#include <gdk/gdk.h>

foreign import ccall "gdk_device_get_name" c_gdk_device_get_name ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetName :: GdkDevice -> IO String
gdkDeviceGetName (GdkDevice p) = peekCString =<< c_gdk_device_get_name p

foreign import ccall "gdk_device_get_vendor_id" c_gdk_device_get_vendor_id ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetVendorId :: GdkDevice -> IO (Maybe String)
gdkDeviceGetVendorId (GdkDevice p) = c_gdk_device_get_vendor_id p >>= \case
	cs	| cs == nullPtr -> pure Nothing
		| otherwise -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_product_id" c_gdk_device_get_product_id ::
	Ptr GdkDevice -> IO CString

gdkDeviceGetProductId :: GdkDevice -> IO (Maybe String)
gdkDeviceGetProductId (GdkDevice p) = c_gdk_device_get_product_id p >>= \case
	cs	| cs == nullPtr -> pure Nothing
		| otherwise -> Just <$> peekCString cs

foreign import ccall "gdk_device_get_source" c_gdk_device_get_source ::
	Ptr GdkDevice -> IO #type GdkInputSource

gdkDeviceGetSource :: GdkDevice -> IO GdkInputSource
gdkDeviceGetSource (GdkDevice p) = GdkInputSource <$> c_gdk_device_get_source p
