{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk.Cairo (
	cairoSetSourceRgb,
	cairoStroke, cairoStrokePreserve, cairoFill,
	cairoMoveTo, cairoLineTo, cairoRectangle,
	cairoShowText, cairoSetFontSize, cairoSelectFontFace,
	CairoSurfaceT,
	cairoImageSurfaceCreateFromPng, cairoSurfaceDestroy, cairoWithImageSurfaceFromPng,
	cairoSetSourceSurface, cairoPaint
	) where

import Foreign.Ptr
import Foreign.C
import Control.Exception
import Data.Word

import Graphics.Gtk.CairoType
import Graphics.Gtk.Cairo.Values
import Graphics.Gtk.AsPointer

#include <cairo.h>

foreign import ccall "cairo_move_to" c_cairo_move_to :: Ptr CairoT -> #{type double} -> #{type double} -> IO ()
foreign import ccall "cairo_line_to" c_cairo_line_to :: Ptr CairoT -> #{type double} -> #{type double} -> IO ()
foreign import ccall "cairo_stroke" c_cairo_stroke :: Ptr CairoT -> IO ()

cairoMoveTo, cairoLineTo :: CairoT -> #{type double} -> #{type double} -> IO ()
cairoMoveTo (CairoT cr) = c_cairo_move_to cr
cairoLineTo (CairoT cr) = c_cairo_line_to cr

cairoStroke :: CairoT -> IO ()
cairoStroke (CairoT cr) = c_cairo_stroke cr

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

foreign import ccall "cairo_show_text" c_cairo_show_text :: Ptr CairoT -> CString -> IO ()

cairoShowText :: CairoT -> String -> IO ()
cairoShowText (CairoT cr) s = withCString s $ c_cairo_show_text cr

foreign import ccall "cairo_set_font_size" c_cairo_set_font_size ::
	Ptr CairoT -> #{type double} -> IO ()


cairoSetFontSize :: CairoT -> #{type double} -> IO ()
cairoSetFontSize (CairoT cr) = c_cairo_set_font_size cr

foreign import ccall "cairo_select_font_face" c_cairo_select_font_face ::
	Ptr CairoT -> CString -> #{type cairo_font_slant_t} -> #{type cairo_font_weight_t} -> IO ()

cairoSelectFontFace :: CairoT -> String -> CairoFontSlantT -> CairoFontWeightT -> IO ()
cairoSelectFontFace (CairoT cr) fn (CairoFontSlantT sl) (CairoFontWeightT w) =
	withCString fn \cfn -> c_cairo_select_font_face cr cfn sl w

newtype CairoSurfaceT = CairoSurfaceT (Ptr CairoSurfaceT) deriving Show

instance AsPointer CairoSurfaceT where
	asPointer (CairoSurfaceT p) = ($ p)
	asValue = pure . CairoSurfaceT

foreign import ccall "cairo_image_surface_create_from_png" c_cairo_image_surface_create_from_png ::
	CString -> IO (Ptr CairoSurfaceT)

cairoImageSurfaceCreateFromPng :: FilePath -> IO CairoSurfaceT
cairoImageSurfaceCreateFromPng fp = withCString fp $ (CairoSurfaceT <$>) . c_cairo_image_surface_create_from_png

foreign import ccall "cairo_surface_destroy" c_cairo_surface_destroy :: Ptr CairoSurfaceT -> IO ()

cairoSurfaceDestroy :: CairoSurfaceT -> IO ()
cairoSurfaceDestroy (CairoSurfaceT s) = c_cairo_surface_destroy s

cairoWithImageSurfaceFromPng  :: FilePath -> (CairoSurfaceT -> IO a) -> IO a
cairoWithImageSurfaceFromPng fp =
	bracket (cairoImageSurfaceCreateFromPng fp) cairoSurfaceDestroy

foreign import ccall "cairo_set_source_surface" c_cairo_set_source_surface ::
	Ptr CairoT -> Ptr CairoSurfaceT -> #{type double} -> #{type double} -> IO ()

cairoSetSourceSurface :: CairoT -> CairoSurfaceT -> #{type double} -> #{type double} -> IO ()
cairoSetSourceSurface (CairoT cr) (CairoSurfaceT s) x y = c_cairo_set_source_surface cr s x y

foreign import ccall "cairo_paint" c_cairo_paint :: Ptr CairoT -> IO ()

cairoPaint :: CairoT -> IO ()
cairoPaint (CairoT cr) = c_cairo_paint cr
