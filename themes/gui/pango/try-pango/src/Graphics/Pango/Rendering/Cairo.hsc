{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Rendering.Cairo where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Control.Monad.Primitive

import Graphics.Pango.Types

import Data.CairoContext

import Graphics.Pango.Basic.Rendering
import Graphics.Pango.Basic.LayoutObjects.PangoLayout

pangoCairoCreateContext :: CairoT RealWorld -> IO PangoContext
pangoCairoCreateContext (CairoT fcr) = withForeignPtr fcr \cr ->
	mkPangoContext =<< c_pango_cairo_create_context cr

foreign import ccall "pango_cairo_create_context"
	c_pango_cairo_create_context :: Ptr (CairoT s) -> IO (Ptr PangoContext)

foreign import ccall "pango_cairo_update_context"
	c_pango_cairo_update_context :: Ptr (CairoT s) -> Ptr PangoContext -> IO ()

pangoCairoUpdateContext :: CairoT RealWorld -> PangoContext -> IO ()
pangoCairoUpdateContext (CairoT fcr) (PangoContext fpc) =
	withForeignPtr fcr \cr -> withForeignPtr fpc \pc ->
		c_pango_cairo_update_context cr pc

pangoCairoCreateLayout :: CairoT RealWorld -> IO PangoLayout
pangoCairoCreateLayout (CairoT fcr) =
	withForeignPtr fcr \cr -> mkPangoLayout =<< c_pango_cairo_create_layout cr

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr (CairoT s) -> IO (Ptr PangoLayout)

foreign import ccall "pango_cairo_update_layout" c_pango_cairo_update_layout ::
	Ptr (CairoT s) -> Ptr PangoLayout -> IO ()

pangoCairoUpdateLayout :: CairoT RealWorld -> PangoLayout -> IO ()
pangoCairoUpdateLayout (CairoT fcr) (PangoLayout fpl) =
	withForeignPtr fcr \cr -> withForeignPtr fpl \pl ->
		c_pango_cairo_update_layout cr pl

foreign import ccall "pango_cairo_show_glyph_item"
	c_pango_cairo_show_glyph_item ::
	Ptr (CairoT s) -> CString -> Ptr PangoGlyphItem -> IO ()

pangoCairoShowGlyphItem :: PrimMonad m =>
	CairoT (PrimState m) -> String -> PangoGlyphItem -> m ()
pangoCairoShowGlyphItem (CairoT fcr) txt (PangoGlyphItem fpgi) = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withCString txt \ctxt -> withForeignPtr fpgi \pgi ->
		c_pango_cairo_show_glyph_item cr ctxt pgi

foreign import ccall "pango_cairo_show_layout_line"
	c_pango_cairo_show_layout_line ::
	Ptr (CairoT s) -> Ptr PangoLayoutLine -> IO()

pangoCairoShowLayoutLine :: PrimMonad m =>
	CairoT (PrimState m) -> PangoLayoutLine -> m ()
pangoCairoShowLayoutLine (CairoT fcr) (PangoLayoutLine fpll) = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fpll \pll ->
		c_pango_cairo_show_layout_line cr pll

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr (CairoT s) -> Ptr PangoLayout -> IO ()

pangoCairoShowLayout :: CairoT RealWorld -> PangoLayout -> IO ()
pangoCairoShowLayout (CairoT fcr) (PangoLayout fpl) =
	withForeignPtr fcr \cr -> withForeignPtr fpl \pl ->
		c_pango_cairo_show_layout cr pl

foreign import ccall "pango_cairo_show_error_underline" c_pango_cairo_show_error_underline ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

pangoCairoShowErrorUnderline :: PrimMonad m =>
	CairoT (PrimState m) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
pangoCairoShowErrorUnderline (CairoT fcr) x y w h = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> c_pango_cairo_show_error_underline cr x y w h

foreign import ccall "pango_cairo_layout_line_path" c_pango_cairo_layout_line_path ::
	Ptr (CairoT s) -> Ptr PangoLayoutLine -> IO ()

pangoCairoLayoutLinePath :: PrimMonad m =>
	CairoT (PrimState m) -> PangoLayoutLine -> m ()
pangoCairoLayoutLinePath (CairoT fcr) (PangoLayoutLine fpll) = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fpll \pll ->
		c_pango_cairo_layout_line_path cr pll

foreign import ccall "pango_cairo_layout_path" c_pango_cairo_layout_path ::
	Ptr (CairoT s) -> Ptr PangoLayout -> IO ()

pangoCairoLayoutPath :: PrimMonad m =>
	CairoT (PrimState m) -> PangoLayout	-> m ()
pangoCairoLayoutPath (CairoT fcr) (PangoLayout fpl) = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> withForeignPtr fpl \pl ->
		c_pango_cairo_layout_path cr pl

foreign import ccall "pango_cairo_error_underline_path" c_pango_cairo_error_underline_path ::
	Ptr (CairoT s) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> IO ()

pangoCairoErrorUnderlinePath :: PrimMonad m =>
	CairoT (PrimState m) -> #{type double} -> #{type double} -> #{type double} -> #{type double} -> m ()
pangoCairoErrorUnderlinePath (CairoT fcr) x y w h = unsafeIOToPrim
	$ withForeignPtr fcr \cr -> c_pango_cairo_error_underline_path cr x y w h
