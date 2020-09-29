{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk.Pango where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Int
import Graphics.CairoType

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

#include <pango/pango.h>

newtype PangoLayout = PangoLayout (Ptr PangoLayout) deriving Show

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr CairoT -> IO (Ptr PangoLayout)

pangoCairoCreateLayout :: CairoT -> IO PangoLayout
pangoCairoCreateLayout (CairoT cr) = PangoLayout <$> c_pango_cairo_create_layout cr

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr PangoLayout -> CString -> #{type int} -> IO ()

pangoLayoutSetText :: PangoLayout -> T.Text -> IO ()
pangoLayoutSetText (PangoLayout l) txt =
	BS.useAsCString (T.encodeUtf8 txt) \cs -> c_pango_layout_set_text l cs (- 1)

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr CairoT -> Ptr PangoLayout -> IO ()

pangoCairoShowLayout :: CairoT -> PangoLayout -> IO ()
pangoCairoShowLayout (CairoT cr) (PangoLayout l) = c_pango_cairo_show_layout cr l

newtype PangoFontDescription = PangoFontDescription (Ptr PangoFontDescription)
	deriving Show

foreign import ccall "pango_font_description_from_string" c_pango_font_description_from_string ::
	CString -> IO (Ptr PangoFontDescription)

pangoFontDescriptionFromString :: T.Text -> IO PangoFontDescription
pangoFontDescriptionFromString txt =
	BS.useAsCString (T.encodeUtf8 txt) \cs ->
		PangoFontDescription <$> c_pango_font_description_from_string cs

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetFontDescription :: PangoLayout -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayout l) (PangoFontDescription d) =
	c_pango_layout_set_font_description l d

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr PangoFontDescription -> #{type gint} -> IO ()

pangoFontDescriptionSetSize :: PangoFontDescription -> #{type gint} -> IO ()
pangoFontDescriptionSetSize (PangoFontDescription d) =
	c_pango_font_description_set_size d . (* #{const PANGO_SCALE})

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr PangoFontDescription -> #{type double} -> IO ()

pangoFontDescriptionSetAbsoluteSize :: PangoFontDescription -> #{type double} -> IO ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescription d) =
	c_pango_font_description_set_absolute_size d . (* #{const PANGO_SCALE})

newtype PangoRectangle = PangoRectangle (Ptr PangoRectangle) deriving Show

foreign import ccall "pango_layout_get_extents" c_pango_layout_get_extents ::
	Ptr PangoLayout -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutWithExtents :: PangoLayout -> (PangoRectangle -> PangoRectangle -> IO a) -> IO a
pangoLayoutWithExtents (PangoLayout l) f =
	allocaBytes #{size PangoRectangle} \pir ->
		allocaBytes #{size PangoRectangle} \plr -> do
			c_pango_layout_get_extents l pir plr
			f (PangoRectangle pir) (PangoRectangle plr)

pangoRectangleX, pangoRectangleY, pangoRectangleWidth,
	pangoRectangleHeight :: PangoRectangle -> IO #{type int}
pangoRectangleX (PangoRectangle r) = #{peek PangoRectangle, x} r
pangoRectangleY (PangoRectangle r) = #{peek PangoRectangle, y} r
pangoRectangleWidth (PangoRectangle r) = #{peek PangoRectangle, width} r
pangoRectangleHeight (PangoRectangle r) = #{peek PangoRectangle, height} r

foreign import ccall "pango_layout_get_pixel_extents" c_pango_layout_get_pixel_extents ::
	Ptr PangoLayout -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutWithPixelExtents :: PangoLayout -> (PangoRectangle -> PangoRectangle -> IO a) -> IO a
pangoLayoutWithPixelExtents (PangoLayout l) f =
	allocaBytes #{size PangoRectangle} \pir ->
		allocaBytes #{size PangoRectangle} \plr -> do
			c_pango_layout_get_pixel_extents l pir plr
			f (PangoRectangle pir) (PangoRectangle plr)
