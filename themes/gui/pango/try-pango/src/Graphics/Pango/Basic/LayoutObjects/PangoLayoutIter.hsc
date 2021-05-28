{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
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
pangoLayoutIterNextChar (PangoLayoutIter fpli) =
	gbooleanToBool <$> withForeignPtr fpli c_pango_layout_iter_next_char

foreign import ccall "pango_layout_iter_next_char"
	c_pango_layout_iter_next_char ::
	Ptr PangoLayoutIter -> IO #type gboolean

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool #{const TRUE} = True
gbooleanToBool _ = error "bad gboolean"
