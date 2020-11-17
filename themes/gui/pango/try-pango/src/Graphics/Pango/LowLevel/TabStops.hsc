{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.LowLevel.TabStops where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive
import Data.Int

import Graphics.Pango.Monad
import Graphics.Pango.Types

#include <pango/pango.h>

foreign import ccall "pango_tab_array_new" c_pango_tab_array_new ::
	#{type gint} -> #{type gboolean} -> IO (Ptr (PangoTabArray s))

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

pangoTabArrayNew :: PrimMonad m =>
	#{type gint} -> Bool -> m (PangoTabArray (PrimState m))
pangoTabArrayNew sz px = unPrimIo
	$ makePangoTabArray =<< c_pango_tab_array_new sz (boolToGboolean px)

foreign import ccall "pango_tab_array_get_size" c_pango_tab_array_get_size ::
	Ptr (PangoTabArray s) -> IO #type gint

pangoTabArrayGetSize :: PrimMonad m =>
	PangoTabArray (PrimState m) -> m #type gint
pangoTabArrayGetSize (PangoTabArray fpta) = unPrimIo
	$ withForeignPtr fpta \pta -> c_pango_tab_array_get_size pta

foreign import ccall "pango_tab_array_resize" c_pango_tab_array_resize ::
	Ptr (PangoTabArray s) -> #{type gint} -> IO ()

pangoTabArrayResize :: PrimMonad m =>
	PangoTabArray (PrimState m) -> #{type gint} -> m ()
pangoTabArrayResize (PangoTabArray fpta) sz = unPrimIo
	$ withForeignPtr fpta \pta -> c_pango_tab_array_resize pta sz
