{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Data.Int

import Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine

import Graphics.Pango.Basic.Fonts.PangoFontDescription hiding (gbooleanToBool)

import Graphics.Pango.PangoRectangle

#include <pango/pango.h>

foreign import ccall "pango_layout_line_index_to_x" c_pango_layout_line_index_to_x ::
	Ptr PangoLayoutLine -> #{type int} -> #{type gboolean} -> Ptr #{type int} -> IO ()

pangoLayoutLineIndexToX :: PangoLayoutLine -> #{type int} -> Bool -> IO #type int
pangoLayoutLineIndexToX (PangoLayoutLine fpll) idx trl =
	withForeignPtr fpll \pll -> alloca \xpos -> do
		c_pango_layout_line_index_to_x pll idx (boolToGboolean trl) xpos
		peek xpos

foreign import ccall "pango_layout_line_x_to_index" c_pango_layout_line_x_to_index ::
	Ptr PangoLayoutLine -> #{type int} -> Ptr #{type int} -> Ptr #{type int} -> IO #type gboolean

pangoLayoutLineXToIndex :: PangoLayoutLine -> #{type int} -> IO (#{type int}, #{type int}, Bool)
pangoLayoutLineXToIndex (PangoLayoutLine fpll) xpos =
	withForeignPtr fpll \pll -> alloca \idx -> alloca \trl -> do
		isd <- c_pango_layout_line_x_to_index pll xpos idx trl
		(,,) <$> peek idx <*> peek trl <*> pure (gbooleanToBool isd)

foreign import ccall "pango_layout_line_get_x_ranges" c_pango_layout_line_get_x_ranges ::
	Ptr PangoLayoutLine -> #{type int} -> #{type int} -> Ptr (Ptr #{type int}) -> Ptr #{type int} -> IO ()

foreign import ccall "g_free" c_g_free :: Ptr a -> IO ()

pangoLayoutLineGetXRanges :: PangoLayoutLine -> #{type int} -> #{type int} -> IO [#type int]
pangoLayoutLineGetXRanges (PangoLayoutLine fpll) st ed =
	withForeignPtr fpll \pll -> alloca \prngs -> alloca \pn -> do
		c_pango_layout_line_get_x_ranges pll st ed prngs pn
		rngs <- peek prngs
		n <- peek pn
		peekArray (fromIntegral $ 2 * n) rngs <* c_g_free rngs

{-
foreign import ccall "pango_layout_line_get_height" c_pango_layout_line_get_height ::
	Ptr PangoLayoutLine -> Ptr #{type int} -> IO ()

pangoLayoutLineGetHeight :: PangoLayoutLine -> #{type int}
pangoLayoutLineGetHeight (PangoLayoutLine fpll) =
	withForeignPtr fpll \pll -> alloca \h -> do
		c_pango_layout_line_get_height pll h
		peek h
		-}
