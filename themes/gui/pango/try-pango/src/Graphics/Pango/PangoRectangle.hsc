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

import Graphics.Pango.PangoFixed

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
c_pango_rectangle_copy s = do
	d <- mallocBytes #{size PangoRectangle}
	#{poke PangoRectangle, x} d
		=<< (#{peek PangoRectangle, x} s :: IO CInt)
	#{poke PangoRectangle, y} d
		=<< (#{peek PangoRectangle, y} s :: IO CInt)
	#{poke PangoRectangle, width} d
		=<< (#{peek PangoRectangle, width} s :: IO CInt)
	#{poke PangoRectangle, height} d
		=<< (#{peek PangoRectangle, height} s :: IO CInt)
	pure d

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
		PangoRectangle (fromPangoFixed x) (fromPangoFixed y) (fromPangoFixed w) (fromPangoFixed h)

pangoRectangleFixed ::
	PangoRectangle -> (PangoFixed, PangoFixed, PangoFixed, PangoFixed)
pangoRectangleFixed (PangoRectangle
	(toPangoFixed -> x) (toPangoFixed -> y)
	(toPangoFixed -> w) (toPangoFixed -> h)) = (x, y, w, h)

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
