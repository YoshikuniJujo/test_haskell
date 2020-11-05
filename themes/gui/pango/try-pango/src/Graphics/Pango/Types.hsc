module Graphics.Pango.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

newtype PangoLayout s = PangoLayout (ForeignPtr (PangoLayout s)) deriving Show

makePangoLayout :: Ptr (PangoLayout s) -> IO (PangoLayout s)
makePangoLayout p = PangoLayout <$> newForeignPtr p (c_g_object_unref p)

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype PangoFontDescription s = PangoFontDescription (ForeignPtr (PangoFontDescription s)) deriving Show

makePangoFontDescription :: Ptr (PangoFontDescription s) -> IO (PangoFontDescription s)
makePangoFontDescription p = PangoFontDescription <$> newForeignPtr p (c_pango_font_description_free p)

foreign import ccall "pango_font_description_free" c_pango_font_description_free ::
	Ptr (PangoFontDescription s) -> IO ()
