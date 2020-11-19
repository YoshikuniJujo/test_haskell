{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Rendering.Cairo where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Control.Monad.Primitive
import Control.Monad.ST
import Graphics.Cairo.Types

import Graphics.Pango.Monad
import Graphics.Pango.Types

foreign import ccall "pango_cairo_create_context"
	c_pango_cairo_create_context :: Ptr (CairoT s) -> IO (Ptr PangoContext)

pangoCairoCreateContext :: CairoT RealWorld -> IO PangoContext
pangoCairoCreateContext (CairoT fcr) = withForeignPtr fcr \cr ->
	makePangoContext =<< c_pango_cairo_create_context cr

foreign import ccall "pango_cairo_show_glyph_item"
	c_pango_cairo_show_glyph_item ::
	Ptr (CairoT s) -> CString -> Ptr PangoGlyphItem -> IO ()

pangoCairoShowGlyphItem :: PrimMonad m =>
	CairoT (PrimState m) -> String -> PangoGlyphItem -> m ()
pangoCairoShowGlyphItem (CairoT fcr) txt (PangoGlyphItem fpgi) = unPrimIo
	$ withForeignPtr fcr \cr -> withCString txt \ctxt -> withForeignPtr fpgi \pgi ->
		c_pango_cairo_show_glyph_item cr ctxt pgi

foreign import ccall "pango_cairo_show_layout_line"
	c_pango_cairo_show_layout_line ::
	Ptr (CairoT s) -> Ptr PangoLayoutLine -> IO()

pangoCairoShowLayoutLine :: PrimMonad m =>
	CairoT (PrimState m) -> PangoLayoutLine -> m ()
pangoCairoShowLayoutLine (CairoT fcr) (PangoLayoutLine fpll) = unPrimIo
	$ withForeignPtr fcr \cr -> withForeignPtr fpll \pll ->
		c_pango_cairo_show_layout_line cr pll
