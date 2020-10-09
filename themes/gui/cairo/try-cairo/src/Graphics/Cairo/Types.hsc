{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Data.Word

#include <cairo.h>

newtype CairoT = CairoT (ForeignPtr CairoT) deriving Show

makeCairoT :: Ptr CairoT -> IO CairoT
makeCairoT p = CairoT <$> newForeignPtr p (c_cairo_destroy p)

foreign import ccall "cairo_destroy" c_cairo_destroy :: Ptr CairoT -> IO ()

newtype CairoSurfaceT = CairoSurfaceT (ForeignPtr CairoSurfaceT) deriving Show

makeCairoSurfaceT :: Ptr CairoSurfaceT -> IO CairoSurfaceT
makeCairoSurfaceT p = CairoSurfaceT <$> newForeignPtr p (c_cairo_surface_destroy p)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy ::
	Ptr CairoSurfaceT -> IO ()

newtype CairoStatusT = CairoStatusT #{type cairo_status_t} deriving Show

#enum CairoStatusT, CairoStatusT, CAIRO_STATUS_SUCCESS, \
	CAIRO_STATUS_NO_MEMORY, CAIRO_STATUS_INVALID_RESTORE

newtype CairoPatternT = CairoPatternT (ForeignPtr CairoPatternT) deriving Show

makeCairoPatternT :: Ptr CairoPatternT -> IO CairoPatternT
makeCairoPatternT p = CairoPatternT <$> newForeignPtr p (c_cairo_pattern_destroy p)

foreign import ccall "cairo_pattern_destroy" c_cairo_pattern_destroy ::
	Ptr CairoPatternT -> IO ()

data CairoTextExtentsT = CairoTextExtentsT {
	cairoTextExtentsTXBearing :: #{type double},
	cairoTextExtentsTYBearing :: #{type double},
	cairoTextExtentsTWidth :: #{type double},
	cairoTextExtentsTHeight :: #{type double},
	cairoTextExtentsTXAdvance :: #{type double},
	cairoTextExtentsTYAdvance :: #{type double} } deriving Show

instance Storable CairoTextExtentsT where
	sizeOf _ = #{size cairo_text_extents_t}
	alignment _ = #{alignment cairo_text_extents_t}
	peek p = CairoTextExtentsT
		<$> #{peek cairo_text_extents_t, x_bearing} p
		<*> #{peek cairo_text_extents_t, y_bearing} p
		<*> #{peek cairo_text_extents_t, width} p
		<*> #{peek cairo_text_extents_t, height} p
		<*> #{peek cairo_text_extents_t, x_advance} p
		<*> #{peek cairo_text_extents_t, y_advance} p
	poke p (CairoTextExtentsT xb yb w h xa ya) = do
		#{poke cairo_text_extents_t, x_bearing} p xb
		#{poke cairo_text_extents_t, y_bearing} p yb
		#{poke cairo_text_extents_t, width} p w
		#{poke cairo_text_extents_t, height} p h
		#{poke cairo_text_extents_t, x_advance} p xa
		#{poke cairo_text_extents_t, y_advance} p ya
