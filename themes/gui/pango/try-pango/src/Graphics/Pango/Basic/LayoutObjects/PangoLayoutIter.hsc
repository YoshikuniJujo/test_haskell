{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Data.Int

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

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool #{const TRUE} = True
gbooleanToBool _ = error "bad gboolean"
