{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.PangoRectangle where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Struct

import Graphics.Pango.Basic.GlyphStorage

#include <pango/pango.h>

struct "PangoRectangle" #{size PangoRectangle}
	[	("x", ''CInt, [| #{peek PangoRectangle, x} |],
			[| #{poke PangoRectangle, x} |]),
		("y", ''CInt, [| #{peek PangoRectangle, y} |],
			[| #{poke PangoRectangle, y} |]),
		("width", ''CInt, [| #{peek PangoRectangle, width} |],
			[| #{poke PangoRectangle, width} |]),
		("height", ''CInt, [| #{peek PangoRectangle, height} |],
			[| #{poke PangoRectangle, height} |]) ]
	[''Show]

c_pango_rectangle_copy :: Ptr PangoRectangle -> IO (Ptr PangoRectangle)
c_pango_rectangle_copy s = mallocBytes #{size PangoRectangle} >>= \d ->
	d <$ copyBytes d s #{size PangoRectangle}

c_pango_rectangle_free :: Ptr PangoRectangle -> IO ()
c_pango_rectangle_free = free

structPrim "PangoRectangle" 'c_pango_rectangle_copy 'c_pango_rectangle_free
	[''Show]

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
	extentsInkRect :: PangoRectangle,
	extentsLogicalRect :: PangoRectangle } deriving Show

data PixelExtents = PixelExtents {
	pixelExtentsInkRect :: PangoRectanglePixel,
	pixelExtentsLogicalRect :: PangoRectanglePixel } deriving Show
