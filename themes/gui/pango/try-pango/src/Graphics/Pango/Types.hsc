{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Data.Int

#include <pango/pango.h>

newtype PangoLayout = PangoLayout (ForeignPtr PangoLayout) deriving Show

makePangoLayout0, makePangoLayout :: Ptr PangoLayout -> IO PangoLayout
makePangoLayout0 p = PangoLayout <$> newForeignPtr p (pure ())
makePangoLayout p = PangoLayout <$> newForeignPtr p (c_g_object_unref p)

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype PangoFontDescriptionPrim s = PangoFontDescriptionPrim (ForeignPtr (PangoFontDescriptionPrim s)) deriving Show

makePangoFontDescriptionPrim :: Ptr (PangoFontDescriptionPrim s) -> IO (PangoFontDescriptionPrim s)
makePangoFontDescriptionPrim p = PangoFontDescriptionPrim <$> newForeignPtr p (c_pango_font_description_prim_free p)

foreign import ccall "pango_font_description_free" c_pango_font_description_prim_free ::
	Ptr (PangoFontDescriptionPrim s) -> IO ()

newtype PangoTabArrayPrim s = PangoTabArrayPrim (ForeignPtr (PangoTabArrayPrim s)) deriving Show

makePangoTabArrayPrim :: Ptr (PangoTabArrayPrim s) -> IO (PangoTabArrayPrim s)
makePangoTabArrayPrim p = PangoTabArrayPrim <$> newForeignPtr p (c_pango_tab_array_prim_free p)

foreign import ccall "pango_tab_array_free" c_pango_tab_array_prim_free ::
	Ptr (PangoTabArrayPrim s) -> IO ()

newtype PangoTabArray = PangoTabArray (ForeignPtr PangoTabArray) deriving Show

makePangoTabArray :: Ptr PangoTabArray -> IO PangoTabArray
makePangoTabArray p = PangoTabArray <$> newForeignPtr p (c_pango_tab_array_free p)

foreign import ccall "pango_tab_array_free" c_pango_tab_array_free ::
	Ptr PangoTabArray -> IO ()

data PangoRectangle = PangoRectangle {
	pangoRectangleX, pangoRectangleY :: #{type int},
	pangoRectangleWidth, pangoRectangleHeight :: #{type int} } deriving Show

instance Storable PangoRectangle where
	sizeOf _ = #size PangoRectangle
	alignment _ = #alignment PangoRectangle
	peek p = PangoRectangle
		<$> #{peek PangoRectangle, x} p
		<*> #{peek PangoRectangle, y} p
		<*> #{peek PangoRectangle, width} p
		<*> #{peek PangoRectangle, height} p
	poke p (PangoRectangle x y w h) = do
		#{poke PangoRectangle, x} p x
		#{poke PangoRectangle, y} p y
		#{poke PangoRectangle, width} p w
		#{poke PangoRectangle, height} p h

newtype PangoLayoutLine = PangoLayoutLine (ForeignPtr PangoLayoutLine) deriving Show

makePangoLayoutLine0 :: Ptr PangoLayoutLine -> IO PangoLayoutLine
makePangoLayoutLine0 p = PangoLayoutLine <$> newForeignPtr p (pure ())

makePangoLayoutLine :: Ptr PangoLayoutLine -> IO PangoLayoutLine
makePangoLayoutLine p = PangoLayoutLine <$> newForeignPtr p (c_pango_layout_line_unref p)

foreign import ccall "pango_layout_line_unref" c_pango_layout_line_unref ::
	Ptr PangoLayoutLine -> IO ()

newtype PangoLayoutIter s = PangoLayoutIter (ForeignPtr (PangoLayoutIter s)) deriving Show

makePangoLayoutIter :: Ptr (PangoLayoutIter s) -> IO (PangoLayoutIter s)
makePangoLayoutIter p = PangoLayoutIter <$> newForeignPtr p (c_pango_layout_iter_free p)

foreign import ccall "pango_layout_iter_free" c_pango_layout_iter_free ::
	Ptr (PangoLayoutIter s) -> IO ()

newtype PangoGlyphItem = PangoGlyphItem (ForeignPtr PangoGlyphItem) deriving Show

makePangoGlyphItem0, makePangoGlyphItem :: Ptr PangoGlyphItem -> IO PangoGlyphItem
makePangoGlyphItem0 p = PangoGlyphItem <$> newForeignPtr p (pure ())
makePangoGlyphItem p = PangoGlyphItem <$> newForeignPtr p (c_pango_glyph_item_free p)

makePangoGlyphItemMaybe0 :: Ptr PangoGlyphItem -> IO (Maybe PangoGlyphItem)
makePangoGlyphItemMaybe0 p
	| p == nullPtr = pure Nothing
	| otherwise = Just . PangoGlyphItem <$> newForeignPtr p (pure ())

foreign import ccall "pango_glyph_item_free" c_pango_glyph_item_free ::
	Ptr PangoGlyphItem -> IO ()

type PangoLayoutRun = PangoGlyphItem
