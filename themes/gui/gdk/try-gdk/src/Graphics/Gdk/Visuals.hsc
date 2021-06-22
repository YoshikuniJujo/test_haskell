{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Visuals where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Enum
import Data.Int
import Data.Word

import Graphics.Gdk.Types

#include <gdk/gdk.h>

enum "GdkVisualType" ''#{type GdkVisualType} [''Show, ''Eq] [
	("GdkVisualStaticGray", #{const GDK_VISUAL_STATIC_GRAY}),
	("GdkVisualGrayscale", #{const GDK_VISUAL_GRAYSCALE}),
	("GdkVisualStaticColor", #{const GDK_VISUAL_STATIC_COLOR}),
	("GdkVisualPseudoColor", #{const GDK_VISUAL_PSEUDO_COLOR}),
	("GdkVisualTrueColor", #{const GDK_VISUAL_TRUE_COLOR}),
	("GdkVisualDirectColor", #{const GDK_VISUAL_DIRECT_COLOR}) ]

foreign import ccall "gdk_visual_get_depth" c_gdk_visual_get_depth ::
	Ptr GdkVisual -> IO #type gint

gdkVisualGetDepth :: GdkVisual -> IO #type gint
gdkVisualGetDepth (GdkVisual p) = c_gdk_visual_get_depth p

foreign import ccall "gdk_visual_get_visual_type" c_gdk_visual_get_visual_type ::
	Ptr GdkVisual -> IO #type GdkVisualType

gdkVisualGetVisualType :: GdkVisual -> IO GdkVisualType
gdkVisualGetVisualType (GdkVisual p) = GdkVisualType <$> c_gdk_visual_get_visual_type p

foreign import ccall "gdk_visual_get_red_pixel_details" c_gdk_visual_get_red_pixel_details ::
	Ptr GdkVisual -> Ptr #{type guint32} -> Ptr #{type gint} -> Ptr #{type gint} -> IO ()

gdkVisualGetRedPixelDetails :: GdkVisual -> IO (#{type guint32}, #{type gint}, #{type gint})
gdkVisualGetRedPixelDetails (GdkVisual v) = alloca \m -> alloca \s -> alloca \p -> do
	c_gdk_visual_get_red_pixel_details v m s p
	(,,) <$> peek m <*> peek s <*> peek p

foreign import ccall "gdk_visual_get_green_pixel_details" c_gdk_visual_get_green_pixel_details ::
	Ptr GdkVisual -> Ptr #{type guint32} -> Ptr #{type gint} -> Ptr #{type gint} -> IO ()

gdkVisualGetGreenPixelDetails :: GdkVisual -> IO (#{type guint32}, #{type gint}, #{type gint})
gdkVisualGetGreenPixelDetails (GdkVisual v) = alloca \m -> alloca \s -> alloca \p -> do
	c_gdk_visual_get_green_pixel_details v m s p
	(,,) <$> peek m <*> peek s <*> peek p

foreign import ccall "gdk_visual_get_blue_pixel_details" c_gdk_visual_get_blue_pixel_details ::
	Ptr GdkVisual -> Ptr #{type guint32} -> Ptr #{type gint} -> Ptr #{type gint} -> IO ()

gdkVisualGetBluePixelDetails :: GdkVisual -> IO (#{type guint32}, #{type gint}, #{type gint})
gdkVisualGetBluePixelDetails (GdkVisual v) = alloca \m -> alloca \s -> alloca \p -> do
	c_gdk_visual_get_blue_pixel_details v m s p
	(,,) <$> peek m <*> peek s <*> peek p

foreign import ccall "gdk_visual_get_screen" c_gdk_visual_get_screen ::
	Ptr GdkVisual -> IO (Ptr GdkScreen)

gdkVisualGetScreen :: GdkVisual -> IO GdkScreen
gdkVisualGetScreen (GdkVisual p) = GdkScreen <$> c_gdk_visual_get_screen p
