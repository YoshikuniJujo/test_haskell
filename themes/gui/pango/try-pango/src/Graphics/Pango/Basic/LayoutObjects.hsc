{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Data.Int

import Graphics.Pango.Bool
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine

#include <pango/pango.h>

foreign import ccall "pango_layout_line_x_to_index" c_pango_layout_line_x_to_index ::
	Ptr PangoLayoutLine -> CInt -> Ptr CInt -> Ptr CInt -> IO #type gboolean

pangoLayoutLineXToIndex :: PangoLayoutLine -> CInt -> IO (CInt, CInt, Bool)
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
