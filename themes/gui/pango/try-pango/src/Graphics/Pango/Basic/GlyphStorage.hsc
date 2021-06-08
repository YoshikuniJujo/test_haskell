{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage where

import GHC.Stack
import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Control.Exception
import Data.Fixed
import System.IO.Unsafe

#include <pango/pango.h>

data PU

instance HasResolution PU where resolution _ = #{const PANGO_SCALE}

type PangoFixed = Fixed PU

minCInt, maxCInt :: Integer
minCInt = fromIntegral (minBound @CInt)
maxCInt = fromIntegral (maxBound @CInt)

fromCInt :: CInt -> PangoFixed
fromCInt = MkFixed . fromIntegral

toCInt :: HasCallStack => PangoFixed -> CInt
toCInt (MkFixed i)
	| minCInt <= i && i <= maxCInt = fromIntegral i
	| otherwise = unsafePerformIO do
		putStrLn $ prettyCallStack callStack
		throw Overflow

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
