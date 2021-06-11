{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage.Internal (
	-- * PANGO FIXED
	PangoFixed, PU, fromCInt, toCInt,

	-- * PANGO RECTANGLE AND EXTENTS
	-- ** PangoRectangleFixed
	PangoRectangleFixed(..),
	pattern PangoRectangleFixed,
	pangoRectangleFixedX, pangoRectangleFixedY,
	pangoRectangleFixedWidth, pangoRectangleFixedHeight,

	PangoRectangleFixedPrim(..), PangoRectangleFixedST, PangoRectangleFixedIO,
	pangoRectangleFixedFreeze, pangoRectangleFixedThaw, pangoRectangleFixedCopy,

	-- ** PangoRectanglePixel
	PangoRectanglePixel(..),
	pattern PangoRectanglePixel,
	pangoRectanglePixelX, pangoRectanglePixelY,
	pangoRectanglePixelWidth, pangoRectanglePixelHeight,

	PangoRectanglePixelPrim(..),
	PangoRectanglePixelST, PangoRectanglePixelIO,
	pangoRectanglePixelFreeze, pangoRectanglePixelThaw,
	pangoRectanglePixelCopy,

	-- ** Extents and PixelExtents
	Extents(..), PixelExtents(..),
	pangoExtentsToPixelsInclusive, pangoExtentsToPixelsNearest,

	-- * PANGO GLYPH ITEM
	PangoGlyphItem(..), PangoLayoutRun,
	mkPangoGlyphItemMaybe0
	) where

import GHC.Stack
import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Struct
import Control.Monad.Primitive
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

struct "PangoRectangleFixed" #{size PangoRectangle}
	[	("x", ''PangoFixed, [| (fromCInt <$>) . #{peek PangoRectangle, x} |],
			[| \p -> #{poke PangoRectangle, x} p . toCInt |]),
		("y", ''PangoFixed, [| (fromCInt <$>) . #{peek PangoRectangle, y} |],
			[| \p -> #{poke PangoRectangle, y} p . toCInt |]),
		("width", ''PangoFixed, [| (fromCInt <$>) . #{peek PangoRectangle, width} |],
			[| \p -> #{poke PangoRectangle, width} p . toCInt |]),
		("height", ''PangoFixed, [| (fromCInt <$>) . #{peek PangoRectangle, height} |],
			[| \p -> #{poke PangoRectangle, height} p . toCInt |]) ]
	[''Show]

c_pango_rectangle_copy :: Ptr PangoRectangleFixed -> IO (Ptr PangoRectangleFixed)
c_pango_rectangle_copy s = mallocBytes #{size PangoRectangle} >>= \d ->
	d <$ copyBytes d s #{size PangoRectangle}

c_pango_rectangle_free :: Ptr PangoRectangleFixed -> IO ()
c_pango_rectangle_free = free

structPrim "PangoRectangleFixed" 'c_pango_rectangle_copy 'c_pango_rectangle_free
	[''Show]

{-
pattern PangoRectangleFixed ::
	PangoFixed -> PangoFixed -> PangoFixed -> PangoFixed -> PangoRectangle
pattern PangoRectangleFixed {
	pangoRectangleFixedX, pangoRectangleFixedY,
	pangoRectangleFixedWidth, pangoRectangleFixedHeight } <- (pangoRectangleFixed -> (
	pangoRectangleFixedX, pangoRectangleFixedY,
	pangoRectangleFixedWidth, pangoRectangleFixedHeight )) where
	PangoRectangleFixed x y w h =
		PangoRectangle (toCInt x) (toCInt y) (toCInt w) (toCInt h)

pangoRectangleFixed ::
	PangoRectangle -> (PangoFixed, PangoFixed, PangoFixed, PangoFixed)
pangoRectangleFixed (PangoRectangle
	(fromCInt -> x) (fromCInt -> y)
	(fromCInt -> w) (fromCInt -> h)) = (x, y, w, h)
	-}

struct "PangoRectanglePixel" #{size PangoRectangle}
	[	("x", ''CInt, [| #{peek PangoRectangle, x} |],
			[| #{poke PangoRectangle, x} |]),
		("y", ''CInt, [| #{peek PangoRectangle, y} |],
			[| #{poke PangoRectangle, y} |]),
		("width", ''CInt, [| #{peek PangoRectangle, width} |],
			[| #{poke PangoRectangle, width} |]),
		("height", ''CInt, [| #{peek PangoRectangle, height} |],
			[| #{poke PangoRectangle, height} |]) ]
	[''Show]

c_pango_rectangle_pixel_copy :: Ptr PangoRectanglePixel -> IO (Ptr PangoRectanglePixel)
c_pango_rectangle_pixel_copy s = castPtr <$> c_pango_rectangle_copy (castPtr s)

c_pango_rectangle_pixel_free :: Ptr PangoRectanglePixel -> IO ()
c_pango_rectangle_pixel_free = c_pango_rectangle_free . castPtr

structPrim "PangoRectanglePixel" 'c_pango_rectangle_pixel_copy
	'c_pango_rectangle_pixel_free [''Show]

data Extents = Extents {
	extentsInkRect :: PangoRectangleFixed,
	extentsLogicalRect :: PangoRectangleFixed } deriving Show

data PixelExtents = PixelExtents {
	pixelExtentsInkRect :: PangoRectanglePixel,
	pixelExtentsLogicalRect :: PangoRectanglePixel } deriving Show

newtype PangoGlyphItem = PangoGlyphItem (ForeignPtr PangoGlyphItem) deriving Show

mkPangoGlyphItemMaybe0 :: Ptr PangoGlyphItem -> IO (Maybe PangoGlyphItem)
mkPangoGlyphItemMaybe0 = \case
	NullPtr -> pure Nothing
	p -> Just . PangoGlyphItem <$> do
		p' <- c_pango_glyph_item_copy p
		newForeignPtr p' $ c_pango_glyph_item_free p'

foreign import ccall "pango_glyph_item_free" c_pango_glyph_item_free ::
	Ptr PangoGlyphItem -> IO ()

foreign import ccall "pango_glyph_item_copy" c_pango_glyph_item_copy ::
	Ptr PangoGlyphItem -> IO (Ptr PangoGlyphItem)

type PangoLayoutRun = PangoGlyphItem

pangoExtentsToPixelsInclusive ::
	PrimMonad m => PangoRectangleFixedPrim (PrimState m) -> m ()
pangoExtentsToPixelsInclusive (PangoRectangleFixedPrim fr) = unsafeIOToPrim
	$ withForeignPtr fr (`c_pango_extents_to_pixels` nullPtr)

pangoExtentsToPixelsNearest ::
	PrimMonad m => PangoRectangleFixedPrim (PrimState m) -> m ()
pangoExtentsToPixelsNearest (PangoRectangleFixedPrim fr) = unsafeIOToPrim
	. withForeignPtr fr $ c_pango_extents_to_pixels nullPtr

foreign import ccall "pango_extents_to_pixels" c_pango_extents_to_pixels ::
	Ptr PangoRectangleFixed -> Ptr PangoRectangleFixed -> IO ()
