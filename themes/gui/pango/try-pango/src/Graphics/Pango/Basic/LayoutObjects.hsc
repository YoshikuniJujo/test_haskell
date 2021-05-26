{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Monad.Primitive
import Data.Int

import Graphics.Pango.Types
import System.Glib.SinglyLinkedLists

import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter

import Graphics.Pango.Basic.Fonts.PangoFontDescription

import Graphics.Pango.PangoRectangle

#include <pango/pango.h>


pangoLayoutGetLine :: PangoLayout -> CInt -> IO PangoLayoutLine
pangoLayoutGetLine (PangoLayout_ fpl) ln =
	makePangoLayoutLine0 =<< withForeignPtr fpl \pl -> c_pango_layout_get_line_readonly pl ln

foreign import ccall "pango_layout_get_lines_readonly" c_pango_layout_get_lines_readonly ::
	Ptr PangoLayout -> IO (Ptr (GSList PangoLayoutLine))

pangoLayoutGetLines :: PangoLayout -> IO [PangoLayoutLine]
pangoLayoutGetLines (PangoLayout_ fpl) =
	withForeignPtr fpl \pl ->
		mapM makePangoLayoutLine0 =<< g_slist_to_list =<< c_pango_layout_get_lines_readonly pl

foreign import ccall "pango_layout_get_iter" c_pango_layout_get_iter ::
	Ptr PangoLayout -> IO (Ptr PangoLayoutIter)

pangoLayoutGetIter :: PangoLayout -> IO PangoLayoutIter
pangoLayoutGetIter (PangoLayout_ fpl) =
	makePangoLayoutIter =<< withForeignPtr fpl c_pango_layout_get_iter

foreign import ccall "pango_layout_iter_next_run" c_pango_layout_iter_next_run ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterNextRun :: PangoLayoutIter -> IO Bool
pangoLayoutIterNextRun (PangoLayoutIter fpli) =
	gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_run

foreign import ccall "pango_layout_iter_next_char" c_pango_layout_iter_next_char ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterNextChar :: PangoLayoutIter -> IO Bool
pangoLayoutIterNextChar (PangoLayoutIter fpli) =
	gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_char

foreign import ccall "pango_layout_iter_next_cluster" c_pango_layout_iter_next_cluster ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterNextCluster :: PangoLayoutIter -> IO Bool
pangoLayoutIterNextCluster (PangoLayoutIter fpli) =
	gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_cluster

foreign import ccall "pango_layout_iter_next_line" c_pango_layout_iter_next_line ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterNextLine :: PangoLayoutIter -> IO Bool
pangoLayoutIterNextLine (PangoLayoutIter fpli) =
	gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_line

foreign import ccall "pango_layout_iter_at_last_line" c_pango_layout_iter_at_last_line ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterAtLastLine :: PangoLayoutIter -> IO Bool
pangoLayoutIterAtLastLine (PangoLayoutIter fpli) =
	gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_at_last_line

foreign import ccall "pango_layout_iter_get_index" c_pango_layout_iter_get_index ::
	Ptr PangoLayoutIter -> IO #type int

pangoLayoutIterGetIndex :: PangoLayoutIter -> IO #type int
pangoLayoutIterGetIndex (PangoLayoutIter fpli) =
	withForeignPtr fpli c_pango_layout_iter_get_index

foreign import ccall "pango_layout_iter_get_baseline" c_pango_layout_iter_get_baseline ::
	Ptr PangoLayoutIter -> IO #type int

pangoLayoutIterGetBaseline :: PangoLayoutIter -> IO #type int
pangoLayoutIterGetBaseline (PangoLayoutIter fpli) =
	withForeignPtr fpli c_pango_layout_iter_get_baseline

foreign import ccall "pango_layout_iter_get_run_readonly" c_pango_layout_iter_get_run_readonly ::
	Ptr PangoLayoutIter -> IO (Ptr PangoLayoutRun)

pangoLayoutIterGetRun :: PangoLayoutIter -> IO (Maybe PangoLayoutRun)
pangoLayoutIterGetRun (PangoLayoutIter fpli) =
	makePangoGlyphItemMaybe0 =<< withForeignPtr fpli c_pango_layout_iter_get_run_readonly

foreign import ccall "pango_layout_iter_get_line_readonly" c_pango_layout_iter_get_line_readonly ::
	Ptr PangoLayoutIter -> IO (Ptr PangoLayoutLine)

pangoLayoutIterGetLine :: PangoLayoutIter -> IO PangoLayoutLine
pangoLayoutIterGetLine (PangoLayoutIter fpli) =
	makePangoLayoutLine0 =<< withForeignPtr fpli c_pango_layout_iter_get_line_readonly

foreign import ccall "pango_layout_iter_get_layout" c_pango_layout_iter_get_layout ::
	Ptr PangoLayoutIter -> IO (Ptr PangoLayout)

pangoLayoutIterGetLayout :: PangoLayoutIter -> IO PangoLayout
pangoLayoutIterGetLayout (PangoLayoutIter fpli) =
	mkPangoLayout =<< withForeignPtr fpli c_pango_layout_iter_get_layout

foreign import ccall "pango_layout_iter_get_char_extents" c_pango_layout_iter_get_char_extents ::
	Ptr PangoLayoutIter -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetCharExtents :: PangoLayoutIter -> IO PangoRectangle
pangoLayoutIterGetCharExtents (PangoLayoutIter fpli) = unsafeIOToPrim
	$ withForeignPtr fpli \pli -> do
		rct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_char_extents pli rct
		PangoRectangle_ <$> newForeignPtr rct (free rct)


foreign import ccall "pango_layout_iter_get_cluster_extents" c_pango_layout_iter_get_cluster_extents ::
	Ptr PangoLayoutIter -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetClusterExtents :: PangoLayoutIter -> IO (PangoRectangle, PangoRectangle)
pangoLayoutIterGetClusterExtents (PangoLayoutIter fpli) =
	withForeignPtr fpli \pli -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_cluster_extents pli irct lrct
		(,)	<$> (PangoRectangle_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangle_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_iter_get_run_extents" c_pango_layout_iter_get_run_extents ::
	Ptr PangoLayoutIter -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetRunExtents :: PangoLayoutIter -> IO (PangoRectangle, PangoRectangle)
pangoLayoutIterGetRunExtents (PangoLayoutIter fpli) =
	withForeignPtr fpli \pli -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_run_extents pli irct lrct
		(,)	<$> (PangoRectangle_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangle_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_iter_get_line_yrange" c_pango_layout_iter_get_line_yrange ::
	Ptr PangoLayoutIter -> Ptr #{type int} -> Ptr #{type int} -> IO ()

pangoLayoutIterGetLineYrange :: PangoLayoutIter -> IO (#{type int}, #{type int})
pangoLayoutIterGetLineYrange (PangoLayoutIter fpli) =
	withForeignPtr fpli \pli -> alloca \y0 -> alloca \y1 -> do
		c_pango_layout_iter_get_line_yrange pli y0 y1
		(,) <$> peek y0 <*> peek y1

foreign import ccall "pango_layout_iter_get_line_extents" c_pango_layout_iter_get_line_extents ::
	Ptr PangoLayoutIter -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetLineExtents :: PangoLayoutIter -> IO (PangoRectangle, PangoRectangle)
pangoLayoutIterGetLineExtents (PangoLayoutIter fpli) =
	withForeignPtr fpli \pli -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_line_extents pli irct lrct
		(,)	<$> (PangoRectangle_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangle_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_iter_get_layout_extents" c_pango_layout_iter_get_layout_extents ::
	Ptr PangoLayoutIter -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutIterGetLayoutExtents :: PangoLayoutIter -> IO (PangoRectangle, PangoRectangle)
pangoLayoutIterGetLayoutExtents (PangoLayoutIter fpli) =
	withForeignPtr fpli \pli -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_layout_extents pli irct lrct
		(,)	<$> (PangoRectangle_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangle_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_line_get_extents" c_pango_layout_line_get_extents ::
	Ptr PangoLayoutLine -> Ptr PangoRectangle -> Ptr PangoRectangle -> IO ()

pangoLayoutLineGetExtents :: PangoLayoutLine -> IO (PangoRectangle, PangoRectangle)
pangoLayoutLineGetExtents (PangoLayoutLine fpll) =
	withForeignPtr fpll \pll -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_line_get_extents pll irct lrct
		(,)	<$> (PangoRectangle_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangle_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_line_get_pixel_extents" c_pango_layout_line_get_pixel_extents ::
	Ptr PangoLayoutLine -> Ptr PangoRectanglePixel -> Ptr PangoRectanglePixel -> IO ()

pangoLayoutLineGetPixelExtents :: PangoLayoutLine -> IO (PangoRectanglePixel, PangoRectanglePixel)
pangoLayoutLineGetPixelExtents (PangoLayoutLine fpll) =
	withForeignPtr fpll \pll -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_line_get_pixel_extents pll irct lrct
		(,)	<$> (PangoRectanglePixel_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectanglePixel_ <$> newForeignPtr lrct (free lrct))

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
