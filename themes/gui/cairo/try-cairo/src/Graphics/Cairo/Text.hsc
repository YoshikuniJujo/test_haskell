{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Text (
	cairoSelectFontFace, cairoSetFontSize, cairoShowText, cairoTextExtents
	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Graphics.Cairo
import Graphics.Cairo.Types
import Graphics.Cairo.Values

#include <cairo.h>

foreign import ccall "cairo_select_font_face" c_cairo_select_font_face ::
	Ptr CairoT -> CString ->
	#{type cairo_font_slant_t} -> #{type cairo_font_weight_t} -> IO ()

cairoSelectFontFace ::
	CairoT -> T.Text -> CairoFontSlantT -> CairoFontWeightT -> IO ()
cairoSelectFontFace (CairoT fcr) ff (CairoFontSlantT sl) (CairoFontWeightT wt) =
	withForeignPtr fcr \cr -> encode ff \cs ->
		c_cairo_select_font_face cr cs sl wt

foreign import ccall "cairo_set_font_size" c_cairo_set_font_size ::
	Ptr CairoT -> #{type double} -> IO ()

cairoSetFontSize :: CairoT -> #{type double} -> IO ()
cairoSetFontSize (CairoT fcr) = withForeignPtr fcr . flip c_cairo_set_font_size

foreign import ccall "cairo_text_extents" c_cairo_text_extents ::
	Ptr CairoT -> CString -> Ptr CairoTextExtentsT -> IO ()

cairoTextExtents :: CairoT -> T.Text -> IO CairoTextExtentsT
cairoTextExtents (CairoT fcr) t =
	withForeignPtr fcr \cr -> encode t \cs -> alloca \p ->
		c_cairo_text_extents cr cs p *> peek p

encode :: T.Text -> (CString -> IO a) -> IO a
encode t = BS.useAsCString $ T.encodeUtf8 t

foreign import ccall "cairo_show_text" c_cairo_show_text ::
	Ptr CairoT -> CString -> IO ()

cairoShowText :: CairoT -> T.Text -> IO ()
cairoShowText (CairoT fcr) t = withForeignPtr fcr \cr -> encode t \cs ->
	c_cairo_show_text cr cs
