{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Visuals.Internal (
	-- * TYPE
	GdkVisual(..),

	-- * FUNCTION
	gdkVisualGetDepth,
	gdkVisualGetRedPixelDetails,
	gdkVisualGetGreenPixelDetails,
	gdkVisualGetBluePixelDetails,
	gdkVisualGetScreen,

	-- * GDK VISUAL TYPE
	GdkVisualType(..),
	gdkVisualGetVisualType,
	pattern GdkVisualStaticGray, pattern GdkVisualGrayscale,
	pattern GdkVisualStaticColor, pattern GdkVisualPseudoColor,
	pattern GdkVisualTrueColor, pattern GdkVisualDirectColor
) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Data.Word
import System.IO.Unsafe

import {-# SOURCE #-} Graphics.Gdk.GdkScreen.Internal

#include <gdk/gdk.h>

newtype GdkVisual = GdkVisual (Ptr GdkVisual) deriving Show

enum "GdkVisualType" ''#{type GdkVisualType} [''Show, ''Eq] [
	("GdkVisualStaticGray", #{const GDK_VISUAL_STATIC_GRAY}),
	("GdkVisualGrayscale", #{const GDK_VISUAL_GRAYSCALE}),
	("GdkVisualStaticColor", #{const GDK_VISUAL_STATIC_COLOR}),
	("GdkVisualPseudoColor", #{const GDK_VISUAL_PSEUDO_COLOR}),
	("GdkVisualTrueColor", #{const GDK_VISUAL_TRUE_COLOR}),
	("GdkVisualDirectColor", #{const GDK_VISUAL_DIRECT_COLOR}) ]

gdkVisualGetDepth :: GdkVisual -> CInt
gdkVisualGetDepth (GdkVisual p) = unsafePerformIO $ c_gdk_visual_get_depth p

foreign import ccall "gdk_visual_get_depth"
	c_gdk_visual_get_depth :: Ptr GdkVisual -> IO CInt

foreign import ccall "gdk_visual_get_visual_type" c_gdk_visual_get_visual_type ::
	Ptr GdkVisual -> IO #type GdkVisualType

gdkVisualGetVisualType :: GdkVisual -> GdkVisualType
gdkVisualGetVisualType (GdkVisual p) =
	unsafePerformIO $ GdkVisualType <$> c_gdk_visual_get_visual_type p

gdkVisualGetRedPixelDetails :: GdkVisual -> (Word32, CInt, CInt)
gdkVisualGetRedPixelDetails (GdkVisual v) = unsafePerformIO
	$ alloca \m -> alloca \s -> alloca \p -> do
		c_gdk_visual_get_red_pixel_details v m s p
		(,,) <$> peek m <*> peek s <*> peek p

foreign import ccall "gdk_visual_get_red_pixel_details"
	c_gdk_visual_get_red_pixel_details ::
		Ptr GdkVisual -> Ptr Word32 -> Ptr CInt -> Ptr CInt -> IO ()

gdkVisualGetGreenPixelDetails :: GdkVisual -> (Word32, CInt, CInt)
gdkVisualGetGreenPixelDetails (GdkVisual v) =
	unsafePerformIO $ alloca \m -> alloca \s -> alloca \p -> do
		c_gdk_visual_get_green_pixel_details v m s p
		(,,) <$> peek m <*> peek s <*> peek p

foreign import ccall "gdk_visual_get_green_pixel_details" c_gdk_visual_get_green_pixel_details ::
	Ptr GdkVisual -> Ptr Word32 -> Ptr CInt -> Ptr CInt -> IO ()

gdkVisualGetBluePixelDetails :: GdkVisual -> (Word32, CInt, CInt)
gdkVisualGetBluePixelDetails (GdkVisual v) =
	unsafePerformIO $ alloca \m -> alloca \s -> alloca \p -> do
		c_gdk_visual_get_blue_pixel_details v m s p
		(,,) <$> peek m <*> peek s <*> peek p

foreign import ccall "gdk_visual_get_blue_pixel_details" c_gdk_visual_get_blue_pixel_details ::
	Ptr GdkVisual -> Ptr Word32 -> Ptr CInt -> Ptr CInt -> IO ()

gdkVisualGetScreen :: GdkVisual -> GdkScreen
gdkVisualGetScreen (GdkVisual p) =
	unsafePerformIO $ GdkScreen <$> c_gdk_visual_get_screen p

foreign import ccall "gdk_visual_get_screen" c_gdk_visual_get_screen ::
	Ptr GdkVisual -> IO (Ptr GdkScreen)
