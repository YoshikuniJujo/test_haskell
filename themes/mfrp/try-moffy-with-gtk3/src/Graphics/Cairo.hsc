{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo (
	-- * Cairo_t
	-- ** Set Parameter
	cairoSetSourceRgb,
	cairoSetSourceSurface,
	cairoSetLineWidth,
	-- ** Draw
	cairoFill, cairoPaint,
	cairoStroke, cairoStrokePreserve,

	-- * Paths
	cairoLineTo, cairoMoveTo, cairoRectangle,

	-- * Transformations
	cairoScale, cairoIdentityMatrix,

	-- * Surfaces
	-- ** Image Surfaces
	cairoImageSurfaceGetWidth, cairoImageSurfaceGetHeight,
	-- ** PNG Support
	-- *** From File
	cairoImageSurfaceCreateFromPng, cairoWithImageSurfaceFromPng,
	-- *** From Stream
	CairoReadFunc,
	cairoImageSurfaceCreateFromPngStream, cairoWithImageSurfaceFromPngStream,

	-- * Types
	CairoStatusT, cairoStatusSuccess, cairoStatusReadError
	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.C
import Control.Exception
import Data.Word
import Data.Int

import qualified Data.ByteString as BS

import Graphics.CairoType
import Foreign.Tools

#include <cairo.h>

foreign import ccall "cairo_move_to" c_cairo_move_to :: Ptr CairoT -> #{type double} -> #{type double} -> IO ()
foreign import ccall "cairo_line_to" c_cairo_line_to :: Ptr CairoT -> #{type double} -> #{type double} -> IO ()
foreign import ccall "cairo_stroke" c_cairo_stroke :: Ptr CairoT -> IO ()

foreign import ccall "cairo_set_line_width" c_cairo_set_line_width :: Ptr CairoT -> #{type double} -> IO ()

cairoMoveTo, cairoLineTo :: CairoT -> #{type double} -> #{type double} -> IO ()
cairoMoveTo (CairoT cr) = c_cairo_move_to cr
cairoLineTo (CairoT cr) = c_cairo_line_to cr

cairoStroke :: CairoT -> IO ()
cairoStroke (CairoT cr) = c_cairo_stroke cr

cairoSetLineWidth :: CairoT -> #{type double} -> IO ()
cairoSetLineWidth (CairoT cr) = c_cairo_set_line_width cr

foreign import ccall "cairo_rectangle" c_cairo_rectangle :: Ptr CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoRectangle :: CairoT -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoRectangle (CairoT cr) = c_cairo_rectangle cr

foreign import ccall "cairo_set_source_rgb" c_cairo_set_source_rgb ::
	Ptr CairoT -> #{type double} -> #{type double} -> #{type double} -> IO ()

cairoSetSourceRgb :: CairoT -> #{type double} -> #{type double} -> #{type double} -> IO ()
cairoSetSourceRgb (CairoT cr) = c_cairo_set_source_rgb cr

foreign import ccall "cairo_stroke_preserve" c_cairo_stroke_preserve ::
	Ptr CairoT -> IO ()

cairoStrokePreserve :: CairoT -> IO ()
cairoStrokePreserve (CairoT cr) = c_cairo_stroke_preserve cr

foreign import ccall "cairo_fill" c_cairo_fill :: Ptr CairoT -> IO ()

cairoFill :: CairoT -> IO ()
cairoFill (CairoT cr) = c_cairo_fill cr

foreign import ccall "cairo_image_surface_create_from_png" c_cairo_image_surface_create_from_png ::
	CString -> IO (Ptr CairoSurfaceT)

cairoImageSurfaceCreateFromPng :: FilePath -> IO CairoSurfaceT
cairoImageSurfaceCreateFromPng fp = withCString fp \cfp -> do
	p <- c_cairo_image_surface_create_from_png cfp
	CairoSurfaceT <$> setFinalizer p (c_cairo_surface_destroy p)

cairoWithImageSurfaceFromPng  :: FilePath -> (CairoSurfaceT -> IO a) -> IO a
cairoWithImageSurfaceFromPng fp f = withCString fp \cfp ->
	bracket (c_cairo_image_surface_create_from_png cfp) c_cairo_surface_destroy (f . CairoSurfaceT . wrapPtr)

foreign import ccall "cairo_set_source_surface" c_cairo_set_source_surface ::
	Ptr CairoT -> Ptr CairoSurfaceT -> #{type double} -> #{type double} -> IO ()

cairoSetSourceSurface :: CairoT -> CairoSurfaceT -> #{type double} -> #{type double} -> IO ()
cairoSetSourceSurface (CairoT cr) (CairoSurfaceT s_) x y = withPtrForeignPtr s_ \s -> c_cairo_set_source_surface cr s x y

foreign import ccall "cairo_paint" c_cairo_paint :: Ptr CairoT -> IO ()

cairoPaint :: CairoT -> IO ()
cairoPaint (CairoT cr) = c_cairo_paint cr

foreign import ccall "cairo_image_surface_create_from_png_stream"
	c_cairo_image_surface_create_from_png_stream ::
	FunPtr (CCairoReadFunc a) -> Ptr a -> IO (Ptr CairoSurfaceT)

type CCairoReadFunc a = Ptr a -> CString -> #{type unsigned int} -> IO #{type cairo_status_t}
type CairoReadFunc a = a -> #{type unsigned int} -> IO (CairoStatusT, Maybe BS.ByteString)

cairoReadFuncToCCairoReadFunc :: AsPointer a => CairoReadFunc a -> CCairoReadFunc a
cairoReadFuncToCCairoReadFunc f px bf ln = do
	x <- asValue px
	(CairoStatusT st, mbs) <- f x ln
	case mbs of
		Nothing -> pure st
		Just bs	| BS.length bs == fromIntegral ln ->
				st <$ BS.useAsCString bs \cs -> copyArray bf cs (fromIntegral ln)
			| otherwise -> pure let CairoStatusT err = cairoStatusReadError in err

foreign import ccall "wrapper" c_cairo_read_func_t :: CCairoReadFunc a -> IO (FunPtr (CCairoReadFunc a))

cairoImageSurfaceCreateFromPngStream :: AsPointer a => CairoReadFunc a -> a -> IO CairoSurfaceT
cairoImageSurfaceCreateFromPngStream f x = do
	cf <- c_cairo_read_func_t $ cairoReadFuncToCCairoReadFunc f
	asPointer x \px -> do
		p <- c_cairo_image_surface_create_from_png_stream cf px
		CairoSurfaceT <$> setFinalizer p (c_cairo_surface_destroy p)

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy :: Ptr CairoSurfaceT -> IO ()

cairoWithImageSurfaceFromPngStream ::
	AsPointer a => CairoReadFunc a -> a -> (CairoSurfaceT -> IO b) -> IO b
cairoWithImageSurfaceFromPngStream rf x f = asPointer x \px -> bracket
	do	cf <- c_cairo_read_func_t $ cairoReadFuncToCCairoReadFunc rf
		c_cairo_image_surface_create_from_png_stream cf px
	c_cairo_surface_destroy
	(f . CairoSurfaceT . wrapPtr)

foreign import ccall "cairo_image_surface_get_width" c_cairo_image_surface_get_width :: Ptr CairoSurfaceT -> IO #{type int}
foreign import ccall "cairo_image_surface_get_height" c_cairo_image_surface_get_height :: Ptr CairoSurfaceT -> IO #{type int}

cairoImageSurfaceGetWidth, cairoImageSurfaceGetHeight :: CairoSurfaceT -> IO #{type int}
cairoImageSurfaceGetWidth (CairoSurfaceT s) = withPtrForeignPtr s c_cairo_image_surface_get_width
cairoImageSurfaceGetHeight (CairoSurfaceT s) = withPtrForeignPtr s c_cairo_image_surface_get_height

foreign import ccall "cairo_scale" c_cairo_scale :: Ptr CairoT -> #{type double} -> #{type double} -> IO ()

cairoScale :: CairoT -> #{type double} -> #{type double} -> IO ()
cairoScale (CairoT cr) = c_cairo_scale cr

foreign import ccall "cairo_identity_matrix" c_cairo_identity_matrix :: Ptr CairoT -> IO ()

cairoIdentityMatrix :: CairoT -> IO ()
cairoIdentityMatrix (CairoT cr) = c_cairo_identity_matrix cr

newtype CairoStatusT = CairoStatusT #{type cairo_status_t} deriving Show

#enum CairoStatusT, CairoStatusT, CAIRO_STATUS_SUCCESS, CAIRO_STATUS_READ_ERROR
