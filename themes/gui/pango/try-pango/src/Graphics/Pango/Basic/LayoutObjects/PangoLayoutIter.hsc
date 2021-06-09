{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Data.Int

import System.Glib.Bool
import Graphics.Pango.Basic.GlyphStorage.Internal
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.LayoutObjects.PangoLayoutLine

#include <pango/pango.h>

newtype PangoLayoutIter s = PangoLayoutIter (ForeignPtr (PangoLayoutIter s))
	deriving Show

makePangoLayoutIter :: ForeignPtr PangoLayout -> Ptr (PangoLayoutIter s) -> IO (PangoLayoutIter s)
makePangoLayoutIter fl p =
--	PangoLayoutIter <$> newForeignPtr p (c_pango_layout_iter_free p >> touchForeignPtr fl)
	PangoLayoutIter <$> newForeignPtr p (touchForeignPtr fl)

pangoLayoutIterFree :: PangoLayoutIter s -> IO ()
pangoLayoutIterFree (PangoLayoutIter fli) = withForeignPtr fli c_pango_layout_iter_free

foreign import ccall "pango_layout_iter_free" c_pango_layout_iter_free ::
	Ptr (PangoLayoutIter s) -> IO ()

pangoLayoutWithIter :: PangoLayout -> (forall s . PangoLayoutIter s -> IO a) -> IO a
pangoLayoutWithIter l f =
	pangoLayoutGetIter l >>= \li -> f li <* pangoLayoutIterFree li

pangoLayoutGetIter :: PangoLayout -> IO (PangoLayoutIter s)
pangoLayoutGetIter (PangoLayout_ fl) =
	makePangoLayoutIter fl =<< withForeignPtr fl c_pango_layout_get_iter

foreign import ccall "pango_layout_get_iter" c_pango_layout_get_iter ::
	Ptr PangoLayout -> IO (Ptr (PangoLayoutIter s))

pangoLayoutIterNextRun :: PangoLayoutIter s -> IO Bool
pangoLayoutIterNextRun (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_next_run

foreign import ccall "pango_layout_iter_next_run"
	c_pango_layout_iter_next_run ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextChar :: PangoLayoutIter s -> IO Bool
pangoLayoutIterNextChar (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_next_char

foreign import ccall "pango_layout_iter_next_char"
	c_pango_layout_iter_next_char ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextCluster :: PangoLayoutIter s -> IO Bool
pangoLayoutIterNextCluster (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_next_cluster

foreign import ccall "pango_layout_iter_next_cluster"
	c_pango_layout_iter_next_cluster ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterNextLine :: PangoLayoutIter s -> IO Bool
pangoLayoutIterNextLine (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_next_line

foreign import ccall "pango_layout_iter_next_line"
	c_pango_layout_iter_next_line ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterAtLastLine :: PangoLayoutIter s -> IO Bool
pangoLayoutIterAtLastLine (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_at_last_line

foreign import ccall "pango_layout_iter_at_last_line"
	c_pango_layout_iter_at_last_line ::
	Ptr (PangoLayoutIter s) -> IO #type gboolean

pangoLayoutIterGetIndex :: PangoLayoutIter s -> IO CInt
pangoLayoutIterGetIndex (PangoLayoutIter fli) =
	withForeignPtr fli c_pango_layout_iter_get_index

foreign import ccall "pango_layout_iter_get_index"
	c_pango_layout_iter_get_index ::
	Ptr (PangoLayoutIter s) -> IO CInt

pangoLayoutIterGetBaseline :: PangoLayoutIter s -> IO CInt
pangoLayoutIterGetBaseline (PangoLayoutIter fli) =
	withForeignPtr fli c_pango_layout_iter_get_baseline

foreign import ccall "pango_layout_iter_get_baseline"
	c_pango_layout_iter_get_baseline ::
	Ptr (PangoLayoutIter s) -> IO CInt

pangoLayoutIterGetRun :: PangoLayoutIter s -> IO (Maybe PangoLayoutRun)
pangoLayoutIterGetRun (PangoLayoutIter fli) = mkPangoGlyphItemMaybe0
	=<< withForeignPtr fli c_pango_layout_iter_get_run

foreign import ccall "pango_layout_iter_get_run" c_pango_layout_iter_get_run ::
	Ptr (PangoLayoutIter s) -> IO (Ptr PangoLayoutRun)

pangoLayoutIterGetLine :: PangoLayoutIter s -> IO PangoLayoutLine
pangoLayoutIterGetLine (PangoLayoutIter fli) =
	makePangoLayoutLine' fli =<< withForeignPtr fli c_pango_layout_iter_get_line

makePangoLayoutLine' :: ForeignPtr (PangoLayoutIter s) -> Ptr PangoLayoutLine -> IO PangoLayoutLine
makePangoLayoutLine' fli p = PangoLayoutLine <$> newForeignPtr p (touchForeignPtr fli)

foreign import ccall "pango_layout_iter_get_line"
	c_pango_layout_iter_get_line ::
	Ptr (PangoLayoutIter s) -> IO (Ptr PangoLayoutLine)

pangoLayoutIterGetCharExtents :: PangoLayoutIter s -> IO PangoRectangleFixed
pangoLayoutIterGetCharExtents (PangoLayoutIter fli) =
	withForeignPtr fli \pli -> do
		rct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_char_extents pli rct
		PangoRectangleFixed_ <$> newForeignPtr rct (free rct)

foreign import ccall "pango_layout_iter_get_char_extents"
	c_pango_layout_iter_get_char_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangleFixed -> IO ()

pangoLayoutIterGetClusterExtents ::
	(PangoLayoutIter s) -> IO (PangoRectangleFixed, PangoRectangleFixed)
pangoLayoutIterGetClusterExtents (PangoLayoutIter fli) =
	withForeignPtr fli \pli -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_cluster_extents pli irct lrct
		(,)	<$> (PangoRectangleFixed_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangleFixed_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_iter_get_cluster_extents"
	c_pango_layout_iter_get_cluster_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangleFixed -> Ptr PangoRectangleFixed -> IO ()

pangoLayoutIterGetRunExtents ::
	PangoLayoutIter s -> IO (PangoRectangleFixed, PangoRectangleFixed)
pangoLayoutIterGetRunExtents (PangoLayoutIter fli) =
	withForeignPtr fli \pli -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_run_extents pli irct lrct
		(,)	<$> (PangoRectangleFixed_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangleFixed_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_iter_get_run_extents"
	c_pango_layout_iter_get_run_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangleFixed -> Ptr PangoRectangleFixed -> IO ()

pangoLayoutIterGetLineYrange :: PangoLayoutIter s -> IO (CInt, CInt)
pangoLayoutIterGetLineYrange (PangoLayoutIter fli) =
	withForeignPtr fli \pli -> alloca \y0 -> alloca \y1 -> do
		c_pango_layout_iter_get_line_yrange pli y0 y1
		(,) <$> peek y0 <*> peek y1

foreign import ccall "pango_layout_iter_get_line_yrange"
	c_pango_layout_iter_get_line_yrange ::
	Ptr (PangoLayoutIter s) -> Ptr CInt -> Ptr CInt -> IO ()

pangoLayoutIterGetLineExtents ::
	PangoLayoutIter s -> IO (PangoRectangleFixed, PangoRectangleFixed)
pangoLayoutIterGetLineExtents (PangoLayoutIter fli) =
	withForeignPtr fli \pli -> do
		irct <- mallocBytes #{size PangoRectangle}
		lrct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_line_extents pli irct lrct
		(,)	<$> (PangoRectangleFixed_ <$> newForeignPtr irct (free irct))
			<*> (PangoRectangleFixed_ <$> newForeignPtr lrct (free lrct))

foreign import ccall "pango_layout_iter_get_line_extents"
	c_pango_layout_iter_get_line_extents ::
	Ptr (PangoLayoutIter s) -> Ptr PangoRectangleFixed -> Ptr PangoRectangleFixed -> IO ()
