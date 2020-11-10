{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects where

import Foreign.Ptr
import Foreign.ForeignPtr

import Graphics.Pango.Types

foreign import ccall "pango_layout_new" c_pango_layout_new ::
	Ptr PangoContext -> IO (Ptr PangoLayout)

pangoLayoutNew :: PangoContext -> IO PangoLayout
pangoLayoutNew (PangoContext fpc) = withForeignPtr fpc \pc ->
	makePangoLayout =<< c_pango_layout_new pc
