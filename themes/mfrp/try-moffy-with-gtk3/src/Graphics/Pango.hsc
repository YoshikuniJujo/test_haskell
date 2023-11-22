{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango (
	-- * Fonts
	-- ** PangoFontDescription
	-- *** Type
	PangoFontDescription,
	-- *** Make PangoFontDescription
	pangoFontDescriptionNew, pangoWithFontDescription,
	-- *** Set Field
	pangoFontDescriptionSetFamily,
	pangoFontDescriptionSetSize,
	pangoFontDescriptionSetAbsoluteSize,
	-- *** String Representation
	pangoFontDescriptionFromString,

	-- * Layout Objects
	-- ** Type
	PangoLayout,
	-- ** Set Parameters
	pangoLayoutSetText,
	pangoLayoutSetFontDescription,
	-- ** Get Parameters
	pangoLayoutWithPixelExtents,
	pangoLayoutWithExtents,

	-- * Types
	-- ** PangoRectangle
	PangoRectangle,
	pangoRectangleX, pangoRectangleY,
	pangoRectangleWidth, pangoRectangleHeight,

	-- * Cairo Fonts and Rendering
	pangoCairoCreateLayout, pangoCairoWithLayout,
	pangoCairoShowLayout
	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Exception
import Data.Int
import Graphics.CairoType

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Foreign.Tools

#include <pango/pango.h>

newtype PangoFontDescription = PangoFontDescription (PtrForeignPtr PangoFontDescription)
	deriving Show

pangoFontDescriptionNew :: IO PangoFontDescription
pangoFontDescriptionNew = do
	p <- c_pango_font_description_new
	PangoFontDescription <$> setFinalizer p (c_pango_font_description_free p)

foreign import ccall "pango_font_description_new" c_pango_font_description_new ::
	IO (Ptr PangoFontDescription)

foreign import ccall "pango_font_description_free" c_pango_font_description_free ::
	Ptr PangoFontDescription -> IO ()

pangoWithFontDescription :: (PangoFontDescription -> IO a) -> IO a
pangoWithFontDescription f =
	bracket c_pango_font_description_new c_pango_font_description_free $ f . PangoFontDescription . wrapPtr

pangoFontDescriptionSetFamily :: PangoFontDescription -> String -> IO ()
pangoFontDescriptionSetFamily (PangoFontDescription fd) ff = withPtrForeignPtr fd \p -> withCString ff \cs ->
	c_pango_font_description_set_family p cs

foreign import ccall "pango_font_description_set_family" c_pango_font_description_set_family ::
	Ptr PangoFontDescription -> CString -> IO ()

newtype PangoLayout = PangoLayout (PtrForeignPtr PangoLayout) deriving Show

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr CairoT -> IO (Ptr PangoLayout)

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

pangoCairoCreateLayout :: CairoT -> IO PangoLayout
pangoCairoCreateLayout (CairoT cr) = do
	p <- c_pango_cairo_create_layout cr
	PangoLayout <$> setFinalizer p (c_g_object_unref p)

pangoCairoWithLayout :: CairoT -> (PangoLayout -> IO a) -> IO a
pangoCairoWithLayout (CairoT cr) f = bracket
	(c_pango_cairo_create_layout cr) c_g_object_unref (f . PangoLayout . wrapPtr)

foreign import ccall "pango_layout_set_text" c_pango_layout_set_text ::
	Ptr PangoLayout -> CString -> #{type int} -> IO ()

pangoLayoutSetText :: PangoLayout -> T.Text -> IO ()
pangoLayoutSetText (PangoLayout l_) txt = withPtrForeignPtr l_ \l ->
	BS.useAsCString (T.encodeUtf8 txt) \cs -> c_pango_layout_set_text l cs (- 1)

foreign import ccall "pango_cairo_show_layout" c_pango_cairo_show_layout ::
	Ptr CairoT -> Ptr PangoLayout -> IO ()

pangoCairoShowLayout :: CairoT -> PangoLayout -> IO ()
pangoCairoShowLayout (CairoT cr) (PangoLayout l) = withPtrForeignPtr l $ c_pango_cairo_show_layout cr

foreign import ccall "pango_font_description_from_string" c_pango_font_description_from_string ::
	CString -> IO (Ptr PangoFontDescription)

pangoFontDescriptionFromString :: String -> IO PangoFontDescription
pangoFontDescriptionFromString str = withCString str \cs -> do
	p <- c_pango_font_description_from_string cs
	PangoFontDescription <$> setFinalizer p (c_pango_font_description_free p)

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetFontDescription :: PangoLayout -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayout l_) (PangoFontDescription d) = withPtrForeignPtr l_ \l ->
	withPtrForeignPtr d $ c_pango_layout_set_font_description l

foreign import ccall "pango_font_description_set_size" c_pango_font_description_set_size ::
	Ptr PangoFontDescription -> #{type gint} -> IO ()

pangoFontDescriptionSetSize :: PangoFontDescription -> #{type gint} -> IO ()
pangoFontDescriptionSetSize (PangoFontDescription d) sz = withPtrForeignPtr d \p ->
	c_pango_font_description_set_size p $ sz * #{const PANGO_SCALE}

foreign import ccall "pango_font_description_set_absolute_size" c_pango_font_description_set_absolute_size ::
	Ptr PangoFontDescription -> #{type double} -> IO ()

pangoFontDescriptionSetAbsoluteSize :: PangoFontDescription -> #{type double} -> IO ()
pangoFontDescriptionSetAbsoluteSize (PangoFontDescription d) sz = withPtrForeignPtr d \p ->
	c_pango_font_description_set_absolute_size p $ sz * #{const PANGO_SCALE}

newtype PangoRectangle = PangoRectangle (Ptr PangoRectangle) deriving Show

foreign import ccall "pango_layout_get_extents" c_pango_layout_get_extents ::
	Ptr PangoLayout -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutWithExtents :: PangoLayout -> (PangoRectangle -> PangoRectangle -> IO a) -> IO a
pangoLayoutWithExtents (PangoLayout l_) f = withPtrForeignPtr l_ \l ->
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
pangoLayoutWithPixelExtents (PangoLayout l_) f = withPtrForeignPtr l_ \l ->
	allocaBytes #{size PangoRectangle} \pir ->
		allocaBytes #{size PangoRectangle} \plr -> do
			c_pango_layout_get_pixel_extents l pir plr
			f (PangoRectangle pir) (PangoRectangle plr)
