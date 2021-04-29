{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types

import Data.Fixed

#include <pango/pango.h>

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype PangoTabArrayPrim s = PangoTabArrayPrim (ForeignPtr (PangoTabArrayPrim s)) deriving Show

makePangoTabArrayPrim :: Ptr (PangoTabArrayPrim s) -> IO (PangoTabArrayPrim s)
makePangoTabArrayPrim p = PangoTabArrayPrim <$> newForeignPtr p (c_pango_tab_array_prim_free p)

newtype PangoTabArrayDouble s =
	PangoTabArrayDouble (ForeignPtr (PangoTabArrayPrim s)) deriving Show

mkPangoTabArrayDouble ::
	Ptr (PangoTabArrayPrim s) -> IO (PangoTabArrayDouble s)
mkPangoTabArrayDouble p =
	PangoTabArrayDouble <$> newForeignPtr p (c_pango_tab_array_prim_free p)

newtype PangoTabArrayInt s =
	PangoTabArrayInt (ForeignPtr (PangoTabArrayPrim s)) deriving Show

mkPangoTabArrayInt ::
	Ptr (PangoTabArrayPrim s) -> IO (PangoTabArrayInt s)
mkPangoTabArrayInt p =
	PangoTabArrayInt <$> newForeignPtr p (c_pango_tab_array_prim_free p)

foreign import ccall "pango_tab_array_free" c_pango_tab_array_prim_free ::
	Ptr (PangoTabArrayPrim s) -> IO ()

data PangoTabArray
	= PangoTabArrayNull
	| PangoTabArray (ForeignPtr PangoTabArray)
	deriving Show

makePangoTabArray :: Ptr PangoTabArray -> IO PangoTabArray
makePangoTabArray p
	| p == nullPtr = pure PangoTabArrayNull
	| otherwise = PangoTabArray <$> newForeignPtr p (c_pango_tab_array_free p)

foreign import ccall "pango_tab_array_free" c_pango_tab_array_free ::
	Ptr PangoTabArray -> IO ()

data PangoRectangle = PangoRectangle {
	pangoRectangleX, pangoRectangleY :: CInt,
	pangoRectangleWidth, pangoRectangleHeight :: CInt } deriving Show

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

data PangoRectangleFixed = PangoRectangleFixed {
	pangoRectangleFixedX, pangoRectangleFixedY :: PangoFixed,
	pangoRectangleFixedWidth, pangoRectangleFixedHeight :: PangoFixed }
	deriving Show

instance Storable PangoRectangleFixed where
	sizeOf _ = #size PangoRectangle
	alignment _ = #alignment PangoRectangle
	peek p = PangoRectangleFixed
		<$> (toPangoFixed <$> #{peek PangoRectangle, x} p)
		<*> (toPangoFixed <$> #{peek PangoRectangle, y} p)
		<*> (toPangoFixed <$> #{peek PangoRectangle, width} p)
		<*> (toPangoFixed <$> #{peek PangoRectangle, height} p)
	poke p (PangoRectangleFixed x y w h) = do
		#{poke PangoRectangle, x} p $ fromPangoFixed x
		#{poke PangoRectangle, y} p $ fromPangoFixed y
		#{poke PangoRectangle, width} p $ fromPangoFixed w
		#{poke PangoRectangle, height} p $ fromPangoFixed h

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

data PU

instance HasResolution PU where resolution _ = #{const PANGO_SCALE}

type PangoFixed = Fixed PU

toPangoFixed :: CInt -> PangoFixed
toPangoFixed = MkFixed . fromIntegral

fromPangoFixed :: PangoFixed -> CInt
fromPangoFixed (MkFixed i) = fromIntegral i
