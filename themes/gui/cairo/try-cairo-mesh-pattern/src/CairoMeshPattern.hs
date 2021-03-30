{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CairoMeshPattern where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Control.Monad.Primitive

import Graphics.Cairo.Drawing.CairoPatternT.Basic

cairoPatternCreateMesh :: PrimMonad m => m (CairoPatternT (PrimState m))
cairoPatternCreateMesh = returnCairoPatternT c_cairo_pattern_create_mesh

foreign import ccall "cairo_pattern_create_mesh" c_cairo_pattern_create_mesh ::
	IO (Ptr (CairoPatternT s))

cairoMeshPatternBeginPatch :: PrimMonad m => CairoPatternT (PrimState m) -> m ()
cairoMeshPatternBeginPatch (CairoPatternT fpt) =
	unsafeIOToPrim $ withForeignPtr fpt c_cairo_mesh_pattern_begin_patch

foreign import ccall "cairo_mesh_pattern_begin_patch" c_cairo_mesh_pattern_begin_patch ::
	Ptr (CairoPatternT s) -> IO ()

cairoMeshPatternEndPatch :: PrimMonad m => CairoPatternT (PrimState m) -> m ()
cairoMeshPatternEndPatch (CairoPatternT fpt) =
	unsafeIOToPrim $ withForeignPtr fpt c_cairo_mesh_pattern_end_patch

foreign import ccall "cairo_mesh_pattern_end_patch" c_cairo_mesh_pattern_end_patch ::
	Ptr (CairoPatternT s) -> IO ()

cairoMeshPatternMoveTo :: PrimMonad m => CairoPatternT (PrimState m) -> CDouble -> CDouble -> m ()
cairoMeshPatternMoveTo (CairoPatternT fpt) x y =
	unsafeIOToPrim $ withForeignPtr fpt \pt -> c_cairo_mesh_pattern_move_to pt x y

foreign import ccall "cairo_mesh_pattern_move_to" c_cairo_mesh_pattern_move_to ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> IO ()

cairoMeshPatternLineTo :: PrimMonad m => CairoPatternT (PrimState m) -> CDouble -> CDouble -> m ()
cairoMeshPatternLineTo (CairoPatternT fpt) x y =
	unsafeIOToPrim $ withForeignPtr fpt \pt -> c_cairo_mesh_pattern_line_to pt x y

foreign import ccall "cairo_mesh_pattern_line_to" c_cairo_mesh_pattern_line_to ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> IO ()

cairoMeshPatternSetCornerColorRgb :: PrimMonad m => CairoPatternT (PrimState m) -> CUInt -> CDouble -> CDouble -> CDouble -> m ()
cairoMeshPatternSetCornerColorRgb (CairoPatternT fpt) n r g b =
	unsafeIOToPrim $ withForeignPtr fpt \pt -> c_cairo_mesh_pattern_set_corner_color_rgb pt n r g b

foreign import ccall "cairo_mesh_pattern_set_corner_color_rgb" c_cairo_mesh_pattern_set_corner_color_rgb ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> CDouble -> IO ()
