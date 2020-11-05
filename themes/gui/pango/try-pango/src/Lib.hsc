{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Monad.Primitive

import Graphics.Cairo.Types
import Graphics.Pango.Monad
import Graphics.Pango.Types

#include <pango/pango.h>

foreign import ccall "pango_cairo_create_layout" c_pango_cairo_create_layout ::
	Ptr (CairoT s) -> IO (Ptr (PangoLayout s))

pangoCairoCreateLayout :: PrimMonad m => CairoT (PrimState m) -> m (PangoLayout (PrimState m))
pangoCairoCreateLayout (CairoT fcr) = unPrimIo $ withForeignPtr fcr \cr -> makePangoLayout =<< c_pango_cairo_create_layout cr
