module Cairo (
	CairoT(..),
	cairoRectangle,
	cairoFill,
) where

import Foreign.Ptr
import Foreign.C.Types

data CairoT = CairoT (Ptr CairoT)

foreign import ccall "gdk/gdk.h cairo_rectangle" c_cairoRectangle ::
	Ptr CairoT -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
cairoRectangle :: CairoT -> Double -> Double -> Double -> Double -> IO ()
cairoRectangle (CairoT c) x y w h = c_cairoRectangle c x' y' w' h'
	where [x', y', w', h'] = map realToFrac [x, y, w, h]

foreign import ccall "gdk/gdk.h cairo_fill" c_cairoFill :: Ptr CairoT -> IO ()
cairoFill :: CairoT -> IO ()
cairoFill (CairoT c) = c_cairoFill c
