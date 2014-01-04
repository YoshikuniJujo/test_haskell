module Cairo (
	CairoT(..),
	cairoDestroy,
	cairoTranslate,
	cairoRectangle,
	cairoFill,
) where

import Foreign.Ptr
import Foreign.C.Types

data CairoT = CairoT (Ptr CairoT)

foreign import ccall "gdk/gdk.h cairo_destroy" c_cairoDestroy ::
	Ptr CairoT -> IO ()
cairoDestroy :: CairoT -> IO ()
cairoDestroy (CairoT c) = c_cairoDestroy c

foreign import ccall "gdk/gdk.h cairo_translate" c_cairoTranslate ::
	Ptr CairoT -> CDouble -> CDouble -> IO ()
cairoTranslate :: CairoT -> Double -> Double -> IO ()
cairoTranslate (CairoT c) x y = c_cairoTranslate c x' y'
	where [x', y'] = map realToFrac [x, y]

foreign import ccall "gdk/gdk.h cairo_rectangle" c_cairoRectangle ::
	Ptr CairoT -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
cairoRectangle :: CairoT -> Double -> Double -> Double -> Double -> IO ()
cairoRectangle (CairoT c) x y w h = c_cairoRectangle c x' y' w' h'
	where [x', y', w', h'] = map realToFrac [x, y, w, h]

foreign import ccall "gdk/gdk.h cairo_fill" c_cairoFill :: Ptr CairoT -> IO ()
cairoFill :: CairoT -> IO ()
cairoFill (CairoT c) = c_cairoFill c
