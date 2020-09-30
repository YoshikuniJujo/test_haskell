{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango (
	-- * Fonts
	PangoFontDescription,
	pangoFontDescriptionNew, pangoWithFontDescription,
	pangoFontDescriptionFromString,
	pangoFontDescriptionSetSize,
	pangoFontDescriptionSetAbsoluteSize,

	-- * Others
	PangoLayout, PangoRectangle,
	pangoLayoutSetFontDescription,
	pangoLayoutSetText,
	pangoCairoCreateLayout,
	pangoCairoShowLayout,
	pangoRectangleX, pangoRectangleY, pangoRectangleWidth, pangoRectangleHeight,
	pangoLayoutWithPixelExtents,
	pangoLayoutWithExtents
	) where

import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Exception
import Data.Int
import Graphics.CairoType

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

#include <pango/pango.h>

withPtrForeignPtr :: PtrForeignPtr a -> (Ptr a -> IO b) -> IO b
withPtrForeignPtr (Left p) f = f p
withPtrForeignPtr (Right fp) f = withForeignPtr fp f

type PtrForeignPtr a = Either (Ptr a) (ForeignPtr a)

newtype PangoFontDescription = PangoFontDescription (PtrForeignPtr PangoFontDescription)
	deriving Show

pangoFontDescriptionNew :: IO PangoFontDescription
pangoFontDescriptionNew = do
	p <- c_pango_font_description_new
	PangoFontDescription . Right <$> newForeignPtr p (c_pango_font_description_free p)

foreign import ccall "pango_font_description_new" c_pango_font_description_new ::
	IO (Ptr PangoFontDescription)

{-
pangoFontDescriptionFree :: PangoFontDescription -> IO ()
pangoFontDescriptionFree (PangoFontDescription p) = c_pango_font_description_free p
-}

foreign import ccall "pango_font_description_free" c_pango_font_description_free ::
	Ptr PangoFontDescription -> IO ()

pangoWithFontDescription :: (PangoFontDescription -> IO a) -> IO a
pangoWithFontDescription f =
	bracket c_pango_font_description_new c_pango_font_description_free $ f . PangoFontDescription . Left

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

foreign import ccall "pango_font_description_from_string" c_pango_font_description_from_string ::
	CString -> IO (Ptr PangoFontDescription)

pangoFontDescriptionFromString :: T.Text -> IO PangoFontDescription
pangoFontDescriptionFromString txt =
	BS.useAsCString (T.encodeUtf8 txt) \cs -> do
		p <- c_pango_font_description_from_string cs
		PangoFontDescription . Right <$> newForeignPtr p (c_pango_font_description_free p)

foreign import ccall "pango_layout_set_font_description" c_pango_layout_set_font_description ::
	Ptr PangoLayout -> Ptr PangoFontDescription -> IO ()

pangoLayoutSetFontDescription :: PangoLayout -> PangoFontDescription -> IO ()
pangoLayoutSetFontDescription (PangoLayout l) (PangoFontDescription d) = withPtrForeignPtr d
	$ c_pango_layout_set_font_description l

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
