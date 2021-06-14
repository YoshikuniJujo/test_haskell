{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Rendering.Cairo (
	-- * CREATE
	pangoCairoCreateContext,
	pangoCairoUpdateContext,
	pangoCairoCreateLayout,
	pangoCairoUpdateLayout,

	-- * SHOW
	pangoCairoShowLayout,
	pangoCairoShowErrorUnderline,
	pangoCairoShowGlyphItem,
	pangoCairoShowLayoutLine,

	-- * PATH
	pangoCairoLayoutLinePath,
	pangoCairoLayoutPath,
	pangoCairoErrorUnderlinePath ) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C
import Foreign.C.String.Misc
import Control.Monad.Primitive

import Data.CairoContext

import Graphics.Pango.Basic.GlyphStorage.Internal
import Graphics.Pango.Basic.LayoutObjects.PangoLayout.Internal
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine.Internal
import Graphics.Pango.LowLevel.Contexts.Internal

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

pangoCairoCreateContext :: PrimMonad m => CairoT (PrimState m) -> m PangoContext
pangoCairoCreateContext (CairoT fcr) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr ->
		mkPangoContext =<< c_pango_cairo_create_context pcr

foreign import ccall "pango_cairo_create_context"
	c_pango_cairo_create_context :: Ptr (CairoT s) -> IO (Ptr PangoContext)

pangoCairoUpdateContext ::
	PrimMonad m => CairoT (PrimState m) -> PangoContext -> m ()
pangoCairoUpdateContext (CairoT fcr) (PangoContext fc) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> withForeignPtr fc \pc ->
		c_pango_cairo_update_context pcr pc

foreign import ccall "pango_cairo_update_context"
	c_pango_cairo_update_context ::
	Ptr (CairoT s) -> Ptr PangoContext -> IO ()

pangoCairoCreateLayout ::
	PrimMonad m => CairoT (PrimState m) -> m (PangoLayoutPrim (PrimState m))
pangoCairoCreateLayout (CairoT fcr) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr ->
		mkPangoLayoutPrim =<< c_pango_cairo_create_layout pcr

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr (CairoT s) -> IO (Ptr PangoLayout)

pangoCairoUpdateLayout :: PrimMonad m =>
	CairoT (PrimState m) -> PangoLayoutPrim (PrimState m) -> m ()
pangoCairoUpdateLayout (CairoT fcr) (PangoLayoutPrim fl) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> withForeignPtr fl \pl ->
		c_pango_cairo_update_layout pcr pl

foreign import ccall "pango_cairo_update_layout" c_pango_cairo_update_layout ::
	Ptr (CairoT s) -> Ptr PangoLayout -> IO ()

pangoCairoShowGlyphItem :: CairoTIO -> T.Text -> PangoGlyphItem -> IO ()
pangoCairoShowGlyphItem (CairoT fcr) t (PangoGlyphItem fgi) =
	withForeignPtr fcr \cr -> T.withCStringLen t \(ct, n) -> do
		cs' <- copyCString ct n
		addForeignPtrFinalizer fcr $ free cs'
		withForeignPtr fgi $ c_pango_cairo_show_glyph_item cr cs'

foreign import ccall "pango_cairo_show_glyph_item"
	c_pango_cairo_show_glyph_item ::
	Ptr (CairoT s) -> CString -> Ptr PangoGlyphItem -> IO ()

pangoCairoShowLayoutLine :: CairoTIO -> PangoLayoutLine -> IO ()
pangoCairoShowLayoutLine (CairoT fcr) (PangoLayoutLine fll) =
	withForeignPtr fcr \cr ->
		withForeignPtr fll $ c_pango_cairo_show_layout_line cr

foreign import ccall "pango_cairo_show_layout_line"
	c_pango_cairo_show_layout_line ::
	Ptr (CairoT s) -> Ptr PangoLayoutLine -> IO ()

pangoCairoShowLayout :: CairoTIO -> PangoLayout -> IO ()
pangoCairoShowLayout (CairoT fcr) (PangoLayout_ fl) = withForeignPtr fcr \cr ->
	withForeignPtr fl $ c_pango_cairo_show_layout cr

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr (CairoT s) -> Ptr PangoLayout -> IO ()

pangoCairoShowErrorUnderline ::
	CairoTIO -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
pangoCairoShowErrorUnderline (CairoT fcr) x y w h =
	withForeignPtr fcr \cr -> c_pango_cairo_show_error_underline cr x y w h

foreign import ccall "pango_cairo_show_error_underline"
	c_pango_cairo_show_error_underline ::
	Ptr (CairoT s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

pangoCairoLayoutLinePath :: CairoTIO -> PangoLayoutLine -> IO ()
pangoCairoLayoutLinePath (CairoT fcr) (PangoLayoutLine fll) =
	withForeignPtr fcr \cr ->
		withForeignPtr fll $ c_pango_cairo_layout_line_path cr

foreign import ccall "pango_cairo_layout_line_path"
	c_pango_cairo_layout_line_path ::
	Ptr (CairoT s) -> Ptr PangoLayoutLine -> IO ()

pangoCairoLayoutPath :: CairoTIO -> PangoLayout -> IO ()
pangoCairoLayoutPath (CairoT fcr) (PangoLayout_ fl) = withForeignPtr fcr \cr ->
	withForeignPtr fl $ c_pango_cairo_layout_path cr

foreign import ccall "pango_cairo_layout_path" c_pango_cairo_layout_path ::
	Ptr (CairoT s) -> Ptr PangoLayout -> IO ()

pangoCairoErrorUnderlinePath ::
	CairoTIO -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
pangoCairoErrorUnderlinePath (CairoT fcr) x y w h = withForeignPtr fcr \cr ->
	c_pango_cairo_error_underline_path cr x y w h

foreign import ccall "pango_cairo_error_underline_path"
	c_pango_cairo_error_underline_path ::
	Ptr (CairoT s) -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
