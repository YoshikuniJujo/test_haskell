{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.LowLevel.TabStops where

import Foreign.Ptr
import Data.Int

import Graphics.Pango.Types

#include <pango/pango.h>

foreign import ccall "pango_tab_array_new" c_pango_tab_array_new ::
	#{type gint} -> #{type gboolean} -> IO (Ptr PangoTabArray)

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

pangoTabArrayNew :: #{type gint} -> Bool -> IO PangoTabArray
pangoTabArrayNew sz px =
	makePangoTabArray =<< c_pango_tab_array_new sz (boolToGboolean px)
