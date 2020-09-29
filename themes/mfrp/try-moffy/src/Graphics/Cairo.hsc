{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo (
	cairoSetSourceRgb,
	cairoStroke, cairoStrokePreserve, cairoFill,
	cairoSetLineWidth, cairoMoveTo, cairoLineTo, cairoRectangle,
	cairoShowText, cairoSetFontSize, cairoSelectFontFace,
	CairoSurfaceT(..),
	cairoImageSurfaceCreateFromPng, cairoSurfaceDestroy, cairoWithImageSurfaceFromPng,
	cairoSetSourceSurface, cairoPaint,
	CairoReadFunc, cairoImageSurfaceCreateFromPngStream, cairoWithImageSurfaceFromPngStream,
	cairoImageSurfaceGetWidth, cairoImageSurfaceGetHeight,
	cairoScale, cairoIdentityMatrix,
	cairoWithTextExtents,
	cairoTextExtentsXBearing, cairoTextExtentsYBearing,
	cairoTextExtentsWidth, cairoTextExtentsHeight,
	cairoTextExtentsXAdvance, cairoTextExtentsYAdvance
	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Exception
import Data.Word
import Data.Int

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Graphics.Gtk.CairoType
import Graphics.Cairo.Values
import Graphics.Gtk.AsPointer

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
	asPointer x $
		(CairoSurfaceT <$>) . c_cairo_image_surface_create_from_png_stream cf

cairoWithImageSurfaceFromPngStream ::
	AsPointer a => CairoReadFunc a -> a -> (CairoSurfaceT -> IO b) -> IO b
cairoWithImageSurfaceFromPngStream rf x =
	bracket (cairoImageSurfaceCreateFromPngStream rf x) cairoSurfaceDestroy

foreign import ccall "cairo_image_surface_get_width" c_cairo_image_surface_get_width :: Ptr CairoSurfaceT -> IO #{type int}
foreign import ccall "cairo_image_surface_get_height" c_cairo_image_surface_get_height :: Ptr CairoSurfaceT -> IO #{type int}

cairoImageSurfaceGetWidth, cairoImageSurfaceGetHeight :: CairoSurfaceT -> IO #{type int}
cairoImageSurfaceGetWidth (CairoSurfaceT s) = c_cairo_image_surface_get_width s
cairoImageSurfaceGetHeight (CairoSurfaceT s) = c_cairo_image_surface_get_height s

foreign import ccall "cairo_scale" c_cairo_scale :: Ptr CairoT -> #{type double} -> #{type double} -> IO ()

cairoScale :: CairoT -> #{type double} -> #{type double} -> IO ()
cairoScale (CairoT cr) = c_cairo_scale cr

foreign import ccall "cairo_identity_matrix" c_cairo_identity_matrix :: Ptr CairoT -> IO ()

cairoIdentityMatrix :: CairoT -> IO ()
cairoIdentityMatrix (CairoT cr) = c_cairo_identity_matrix cr

newtype CairoTextExtentsT = CairoTextExtentsT (Ptr CairoTextExtentsT) deriving Show

foreign import ccall "cairo_text_extents" c_cairo_text_extents :: Ptr CairoT -> CString -> (Ptr CairoTextExtentsT) -> IO ()

cairoWithTextExtents :: CairoT -> T.Text -> (CairoTextExtentsT -> IO a) -> IO a
cairoWithTextExtents (CairoT cr) txt f = BS.useAsCString (T.encodeUtf8 txt) \cs -> allocaBytes #{size cairo_text_extents_t} \p -> do
	c_cairo_text_extents cr cs p
	f (CairoTextExtentsT p)

cairoTextExtentsXBearing, cairoTextExtentsYBearing :: CairoTextExtentsT -> IO #{type double}
cairoTextExtentsXBearing (CairoTextExtentsT e) = #{peek cairo_text_extents_t, x_bearing} e
cairoTextExtentsYBearing (CairoTextExtentsT e) = #{peek cairo_text_extents_t, y_bearing} e

cairoTextExtentsWidth, cairoTextExtentsHeight :: CairoTextExtentsT -> IO #{type double}
cairoTextExtentsWidth (CairoTextExtentsT e) = #{peek cairo_text_extents_t, width} e
cairoTextExtentsHeight (CairoTextExtentsT e) = #{peek cairo_text_extents_t, height} e

cairoTextExtentsXAdvance, cairoTextExtentsYAdvance :: CairoTextExtentsT -> IO #{type double}
cairoTextExtentsXAdvance (CairoTextExtentsT e) = #{peek cairo_text_extents_t, x_advance} e
cairoTextExtentsYAdvance (CairoTextExtentsT e) = #{peek cairo_text_extents_t, y_advance} e
