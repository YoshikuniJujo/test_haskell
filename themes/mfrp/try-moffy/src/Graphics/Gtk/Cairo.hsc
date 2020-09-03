{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk.Cairo (
	cairoSetSourceRgb,
	cairoStroke, cairoStrokePreserve, cairoFill,
	cairoMoveTo, cairoLineTo, cairoRectangle,
	) where

import Foreign.Ptr

import Graphics.Gtk.CairoType

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
