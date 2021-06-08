{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Types where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

#include <pango/pango.h>

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype PangoTabArrayPrim s = PangoTabArrayPrim (ForeignPtr (PangoTabArrayPrim s)) deriving Show

makePangoTabArrayPrim :: Ptr (PangoTabArrayPrim s) -> IO (PangoTabArrayPrim s)
makePangoTabArrayPrim p = PangoTabArrayPrim <$> newForeignPtr p (c_pango_tab_array_prim_free p)

newtype PangoTabArrayInt s =
	PangoTabArrayInt (ForeignPtr (PangoTabArrayPrim s)) deriving Show

mkPangoTabArrayInt ::
	Ptr (PangoTabArrayPrim s) -> IO (PangoTabArrayInt s)
mkPangoTabArrayInt p =
	PangoTabArrayInt <$> newForeignPtr p (c_pango_tab_array_prim_free p)

newtype PangoTabArrayFixed s =
	PangoTabArrayFixed (ForeignPtr (PangoTabArrayPrim s)) deriving Show

mkPangoTabArrayFixed ::
	Ptr (PangoTabArrayPrim s) -> IO (PangoTabArrayFixed s)
mkPangoTabArrayFixed p =
	PangoTabArrayFixed <$> newForeignPtr p (c_pango_tab_array_prim_free p)

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

newtype PangoGlyphItem = PangoGlyphItem (ForeignPtr PangoGlyphItem) deriving Show

makePangoGlyphItem0, makePangoGlyphItem :: Ptr PangoGlyphItem -> IO PangoGlyphItem
makePangoGlyphItem0 p = PangoGlyphItem <$> newForeignPtr p (pure ())
makePangoGlyphItem p = PangoGlyphItem <$> newForeignPtr p (c_pango_glyph_item_free p)

makePangoGlyphItemMaybe, makePangoGlyphItemMaybe0 ::
	Ptr PangoGlyphItem -> IO (Maybe PangoGlyphItem)
makePangoGlyphItemMaybe = \case
	NullPtr -> pure Nothing
	p -> Just . PangoGlyphItem
		<$> newForeignPtr p (c_pango_glyph_item_free p)

makePangoGlyphItemMaybe0 p
	| p == nullPtr = pure Nothing
	| otherwise = Just . PangoGlyphItem <$> newForeignPtr p (pure ())

foreign import ccall "pango_glyph_item_free" c_pango_glyph_item_free ::
	Ptr PangoGlyphItem -> IO ()

type PangoLayoutRun = PangoGlyphItem
