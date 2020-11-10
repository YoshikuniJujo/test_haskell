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

foreign import ccall "pango_layout_copy" c_pango_layout_copy ::
	Ptr PangoLayout -> IO (Ptr PangoLayout)

pangoLayoutCopy :: PangoLayout -> IO PangoLayout
pangoLayoutCopy (PangoLayout fpl) = withForeignPtr fpl \pl ->
	makePangoLayout =<< c_pango_layout_copy pl
