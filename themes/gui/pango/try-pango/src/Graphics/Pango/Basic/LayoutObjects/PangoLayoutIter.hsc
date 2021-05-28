{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C.Types
import Data.Int

import Graphics.Pango.Types
import Graphics.Pango.PangoRectangle
import Graphics.Pango.Basic.LayoutObjects.PangoLayout

#include <pango/pango.h>

newtype PangoLayoutIter = PangoLayoutIter (ForeignPtr PangoLayoutIter)
	deriving Show

makePangoLayoutIter :: Ptr PangoLayoutIter -> IO PangoLayoutIter
makePangoLayoutIter p =
	PangoLayoutIter <$> newForeignPtr p (c_pango_layout_iter_free p)

foreign import ccall "pango_layout_iter_free" c_pango_layout_iter_free ::
	Ptr PangoLayoutIter -> IO ()

pangoLayoutGetIter :: PangoLayout -> IO PangoLayoutIter
pangoLayoutGetIter (PangoLayout_ fl) =
	makePangoLayoutIter =<< withForeignPtr fl c_pango_layout_get_iter

foreign import ccall "pango_layout_get_iter" c_pango_layout_get_iter ::
	Ptr PangoLayout -> IO (Ptr PangoLayoutIter)

pangoLayoutIterNextRun :: PangoLayoutIter -> IO Bool
pangoLayoutIterNextRun (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_next_run

foreign import ccall "pango_layout_iter_next_run"
	c_pango_layout_iter_next_run ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterNextChar :: PangoLayoutIter -> IO Bool
pangoLayoutIterNextChar (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_next_char

foreign import ccall "pango_layout_iter_next_char"
	c_pango_layout_iter_next_char ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterNextCluster :: PangoLayoutIter -> IO Bool
pangoLayoutIterNextCluster (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_next_cluster

foreign import ccall "pango_layout_iter_next_cluster"
	c_pango_layout_iter_next_cluster ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterNextLine :: PangoLayoutIter -> IO Bool
pangoLayoutIterNextLine (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_next_line

foreign import ccall "pango_layout_iter_next_line"
	c_pango_layout_iter_next_line ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterAtLastLine :: PangoLayoutIter -> IO Bool
pangoLayoutIterAtLastLine (PangoLayoutIter fli) =
	gbooleanToBool <$> withForeignPtr fli c_pango_layout_iter_at_last_line

foreign import ccall "pango_layout_iter_at_last_line"
	c_pango_layout_iter_at_last_line ::
	Ptr PangoLayoutIter -> IO #type gboolean

pangoLayoutIterGetIndex :: PangoLayoutIter -> IO CInt
pangoLayoutIterGetIndex (PangoLayoutIter fli) =
	withForeignPtr fli c_pango_layout_iter_get_index

foreign import ccall "pango_layout_iter_get_index"
	c_pango_layout_iter_get_index ::
	Ptr PangoLayoutIter -> IO CInt

pangoLayoutIterGetBaseline :: PangoLayoutIter -> IO CInt
pangoLayoutIterGetBaseline (PangoLayoutIter fli) =
	withForeignPtr fli c_pango_layout_iter_get_baseline

foreign import ccall "pango_layout_iter_get_baseline"
	c_pango_layout_iter_get_baseline ::
	Ptr PangoLayoutIter -> IO CInt

pangoLayoutIterGetRun :: PangoLayoutIter -> IO (Maybe PangoLayoutRun)
pangoLayoutIterGetRun (PangoLayoutIter fli) = makePangoGlyphItemMaybe
	=<< withForeignPtr fli c_pango_layout_iter_get_run

foreign import ccall "pango_layout_iter_get_run" c_pango_layout_iter_get_run ::
	Ptr PangoLayoutIter -> IO (Ptr PangoLayoutRun)

pangoLayoutIterGetLine :: PangoLayoutIter -> IO PangoLayoutLine
pangoLayoutIterGetLine (PangoLayoutIter fli) =
	makePangoLayoutLine =<< withForeignPtr fli c_pango_layout_iter_get_line

foreign import ccall "pango_layout_iter_get_line"
	c_pango_layout_iter_get_line ::
	Ptr PangoLayoutIter -> IO (Ptr PangoLayoutLine)

pangoLayoutIterGetCharExtents :: PangoLayoutIter -> IO PangoRectangle
pangoLayoutIterGetCharExtents (PangoLayoutIter fli) =
	withForeignPtr fli \pli -> do
		rct <- mallocBytes #{size PangoRectangle}
		c_pango_layout_iter_get_char_extents pli rct
		PangoRectangle_ <$> newForeignPtr rct (free rct)

foreign import ccall "pango_layout_iter_get_char_extents" c_pango_layout_iter_get_char_extents ::
	Ptr PangoLayoutIter -> Ptr PangoRectangle -> IO ()


gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool #{const TRUE} = True
gbooleanToBool _ = error "bad gboolean"
