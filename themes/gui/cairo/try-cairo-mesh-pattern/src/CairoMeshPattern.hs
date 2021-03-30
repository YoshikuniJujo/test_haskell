{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CairoMeshPattern where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Color

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

cairoMeshPatternCurveTo :: PrimMonad m => CairoPatternT (PrimState m) ->
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> m ()
cairoMeshPatternCurveTo (CairoPatternT fpt) x1 y1 x2 y2 x3 y3 =
	unsafeIOToPrim $ withForeignPtr fpt \pt -> c_cairo_mesh_pattern_curve_to pt x1 y1 x2 y2 x3 y3

foreign import ccall "cairo_mesh_pattern_curve_to" c_cairo_mesh_pattern_curve_to ::
	Ptr (CairoPatternT s) -> CDouble -> CDouble -> CDouble -> CDouble ->
	CDouble -> CDouble -> IO ()

cairoMeshPatternSetControlPoint :: PrimMonad m => CairoPatternT (PrimState m) -> CUInt -> CDouble -> CDouble -> m ()
cairoMeshPatternSetControlPoint (CairoPatternT fpt) n x y =
	unsafeIOToPrim $ withForeignPtr fpt \pt -> c_cairo_mesh_pattern_set_control_point pt n x y

foreign import ccall "cairo_mesh_pattern_set_control_point" c_cairo_mesh_pattern_set_control_point ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> IO ()

cairoMeshPatternSetCornerColorRgb :: PrimMonad m => CairoPatternT (PrimState m) -> CUInt -> CDouble -> CDouble -> CDouble -> m ()
cairoMeshPatternSetCornerColorRgb (CairoPatternT fpt) n r g b =
	unsafeIOToPrim $ withForeignPtr fpt \pt -> c_cairo_mesh_pattern_set_corner_color_rgb pt n r g b

foreign import ccall "cairo_mesh_pattern_set_corner_color_rgb" c_cairo_mesh_pattern_set_corner_color_rgb ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> CDouble -> IO ()

cairoMeshPatternSetCornerColorRgba :: PrimMonad m => CairoPatternT (PrimState m) ->
	CUInt -> CDouble -> CDouble -> CDouble -> CDouble -> m ()
cairoMeshPatternSetCornerColorRgba (CairoPatternT fpt) n r g b a =
	unsafeIOToPrim $ withForeignPtr fpt \pt -> c_cairo_mesh_pattern_set_corner_color_rgba pt n r g b a

foreign import ccall "cairo_mesh_pattern_set_corner_color_rgba" c_cairo_mesh_pattern_set_corner_color_rgba ::
	Ptr (CairoPatternT s) -> CUInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

data MoveTo = MoveTo CDouble CDouble deriving Show

data LineCurveTo
	= LineTo CDouble CDouble
	| CurveTo CDouble CDouble CDouble CDouble CDouble CDouble
	deriving Show

data CloseTo
	= CloseLineTo
	| CloseCurveTo CDouble CDouble CDouble CDouble
	deriving Show

data Color = ColorRgb Rgb | ColorRgba Rgba deriving Show

data Point = Point CDouble CDouble deriving Show

cairoMeshPatternLineCurveTo ::
	PrimMonad m => CairoPatternT (PrimState m) -> LineCurveTo -> m ()
cairoMeshPatternLineCurveTo pt = \case
	LineTo x y -> cairoMeshPatternLineTo pt x y
	CurveTo x1 y1 x2 y2 x3 y3 -> cairoMeshPatternCurveTo pt x1 y1 x2 y2 x3 y3

cairoMeshPatternSetCornerColor ::
	PrimMonad m => CairoPatternT (PrimState m) -> CUInt -> Color -> m ()
cairoMeshPatternSetCornerColor pt n = \case
	ColorRgb (RgbDouble r g b) -> cairoMeshPatternSetCornerColorRgb pt n r g b
	ColorRgba (RgbaDouble r g b a) -> cairoMeshPatternSetCornerColorRgba pt n r g b a

cairoMeshPatternCloseTo :: PrimMonad m => CairoPatternT (PrimState m) -> MoveTo -> CloseTo -> m ()
cairoMeshPatternCloseTo pt (MoveTo x0 y0) = \case
	CloseLineTo -> pure ()
	CloseCurveTo x1 y1 x2 y2 -> cairoMeshPatternCurveTo pt x1 y1 x2 y2 x0 y0

cairoMeshPatternSetControlPointMaybe :: PrimMonad m => CairoPatternT (PrimState m) ->
	CUInt -> Maybe Point -> m ()
cairoMeshPatternSetControlPointMaybe pt n = \case
	Nothing -> pure ()
	Just (Point x y) -> cairoMeshPatternSetControlPoint pt n x y

cairoMeshPatternAddPatch :: PrimMonad m => CairoPatternT (PrimState m) ->
	MoveTo -> LineCurveTo -> LineCurveTo -> LineCurveTo -> CloseTo ->
	Color -> Color -> Color -> Color ->
	Maybe Point -> Maybe Point -> Maybe Point -> Maybe Point -> m ()
cairoMeshPatternAddPatch pt mv@(MoveTo x0 y0) lc1 lc2 lc3 cls c0 c1 c2 c3 cp0 cp1 cp2 cp3 = do
	cairoMeshPatternBeginPatch pt
	cairoMeshPatternMoveTo pt x0 y0
	cairoMeshPatternLineCurveTo pt lc1
	cairoMeshPatternLineCurveTo pt lc2
	cairoMeshPatternLineCurveTo pt lc3
	cairoMeshPatternCloseTo pt mv cls
	cairoMeshPatternSetCornerColor pt 0 c0
	cairoMeshPatternSetCornerColor pt 1 c1
	cairoMeshPatternSetCornerColor pt 2 c2
	cairoMeshPatternSetCornerColor pt 3 c3
	cairoMeshPatternSetControlPointMaybe pt 0 cp0
	cairoMeshPatternSetControlPointMaybe pt 1 cp1
	cairoMeshPatternSetControlPointMaybe pt 2 cp2
	cairoMeshPatternSetControlPointMaybe pt 3 cp3
	cairoMeshPatternEndPatch pt
