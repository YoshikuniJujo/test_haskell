{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk.Pango where

import Foreign.Ptr
import Foreign.C
import Data.Int
import Graphics.Gtk.CairoType

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
