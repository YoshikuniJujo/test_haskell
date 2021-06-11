{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine.Internal (

	-- * TYPE
	PangoLayoutLine(..),

	-- * GET LINE
	pangoLayoutGetLine, pangoLayoutGetLines,

	-- * GET FROM LINE
	pangoLayoutLineGetExtents, pangoLayoutLineGetPixelExtents,
	pangoLayoutLineIndexToX, pangoLayoutLineXToIndex,
	pangoLayoutLineGetXRanges ) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Data.Int

import System.Glib.Bool
import Graphics.Pango.Basic.GlyphStorage.Internal
import Graphics.Pango.Basic.LayoutObjects.PangoLayout.Internal
import System.Glib.SinglyLinkedLists

#include <pango/pango.h>

pangoLayoutGetLine :: PangoLayout -> CInt -> IO (Maybe PangoLayoutLine)
pangoLayoutGetLine (PangoLayout_ fl) ln = makePangoLayoutLineMaybe fl
	=<< withForeignPtr fl \pl -> c_pango_layout_get_line pl ln

foreign import ccall "pango_layout_get_line" c_pango_layout_get_line ::
	Ptr PangoLayout -> CInt -> IO (Ptr PangoLayoutLine)

newtype PangoLayoutLine = PangoLayoutLine (ForeignPtr PangoLayoutLine) deriving Show

makePangoLayoutLine :: ForeignPtr PangoLayout -> Ptr PangoLayoutLine -> IO PangoLayoutLine
makePangoLayoutLine fl p = PangoLayoutLine <$> newForeignPtr p (touchForeignPtr fl)

makePangoLayoutLineMaybe :: ForeignPtr PangoLayout -> Ptr PangoLayoutLine -> IO (Maybe PangoLayoutLine)
makePangoLayoutLineMaybe fl = \case
	NullPtr -> pure Nothing
	p -> Just . PangoLayoutLine
		<$> newForeignPtr p (touchForeignPtr fl)

pangoLayoutGetLines :: PangoLayout -> IO [PangoLayoutLine]
pangoLayoutGetLines (PangoLayout_ fl) = withForeignPtr fl \pl ->
	(makePangoLayoutLine fl `mapM`) =<< g_slist_to_list =<< c_pango_layout_get_lines pl

foreign import ccall "pango_layout_get_lines" c_pango_layout_get_lines ::
	Ptr PangoLayout -> IO (Ptr (GSList PangoLayoutLine))

pangoLayoutLineGetExtents :: PangoLayoutLine -> IO Extents
pangoLayoutLineGetExtents (PangoLayoutLine fll) =
	withForeignPtr fll \pll -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_line_get_extents pll irct lrct
		Extents	<$> (PangoRectangleFixed_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangleFixed_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_line_get_extents"
	c_pango_layout_line_get_extents ::
	Ptr PangoLayoutLine -> Ptr PangoRectangleFixed -> Ptr PangoRectangleFixed -> IO ()

pangoLayoutLineGetPixelExtents :: PangoLayoutLine -> IO PixelExtents
pangoLayoutLineGetPixelExtents (PangoLayoutLine fpll) =
	withForeignPtr fpll \pll -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_line_get_pixel_extents pll irct lrct
		PixelExtents
			<$> (PangoRectanglePixel_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectanglePixel_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_line_get_pixel_extents"
	c_pango_layout_line_get_pixel_extents ::
	Ptr PangoLayoutLine ->
	Ptr PangoRectanglePixel -> Ptr PangoRectanglePixel -> IO ()

pangoLayoutLineIndexToX :: PangoLayoutLine -> CInt -> Bool -> IO CInt
pangoLayoutLineIndexToX (PangoLayoutLine fpll) idx trl =
	withForeignPtr fpll \pll -> alloca \xpos -> do
		c_pango_layout_line_index_to_x pll idx (boolToGboolean trl) xpos
		peek xpos

foreign import ccall "pango_layout_line_index_to_x"
	c_pango_layout_line_index_to_x ::
	Ptr PangoLayoutLine -> CInt -> #{type gboolean} -> Ptr CInt -> IO ()

pangoLayoutLineXToIndex :: PangoLayoutLine -> CInt -> IO (CInt, CInt, Bool)
pangoLayoutLineXToIndex (PangoLayoutLine fpll) xpos =
	withForeignPtr fpll \pll -> alloca \idx -> alloca \trl -> do
		isd <- c_pango_layout_line_x_to_index pll xpos idx trl
		(,,) <$> peek idx <*> peek trl <*> pure (gbooleanToBool isd)

foreign import ccall "pango_layout_line_x_to_index"
	c_pango_layout_line_x_to_index ::
	Ptr PangoLayoutLine -> CInt -> Ptr CInt -> Ptr CInt -> IO #type gboolean

pangoLayoutLineGetXRanges :: PangoLayoutLine -> CInt -> CInt -> IO [CInt]
pangoLayoutLineGetXRanges (PangoLayoutLine fpll) st ed =
	withForeignPtr fpll \pll -> alloca \prngs -> alloca \pn -> do
		c_pango_layout_line_get_x_ranges pll st ed prngs pn
		rngs <- peek prngs
		n <- peek pn
		peekArray (fromIntegral $ 2 * n) rngs <* c_g_free rngs

foreign import ccall "pango_layout_line_get_x_ranges" c_pango_layout_line_get_x_ranges ::
	Ptr PangoLayoutLine -> CInt -> CInt -> Ptr (Ptr CInt) -> Ptr CInt -> IO ()

foreign import ccall "g_free" c_g_free :: Ptr a -> IO ()
