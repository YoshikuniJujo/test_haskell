{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

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
