{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutPrim where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Control.Monad.Primitive

import Graphics.Pango.Types

newtype PangoLayoutPrim s = PangoLayoutPrim (ForeignPtr (PangoLayoutPrim s)) deriving Show

mkPangoLayoutPrim :: Ptr (PangoLayoutPrim s) -> IO (PangoLayoutPrim s)
mkPangoLayoutPrim p = PangoLayoutPrim <$> newForeignPtr p (c_g_object_unref p)

pangoLayoutFreeze ::
	PrimMonad m => PangoLayoutPrim (PrimState m) -> m PangoLayout
pangoLayoutFreeze (PangoLayoutPrim fpl) = unsafeIOToPrim
	$ withForeignPtr fpl \ppl -> makePangoLayout =<< c_pango_layout_freeze ppl

foreign import ccall "pango_layout_copy" c_pango_layout_freeze ::
	Ptr (PangoLayoutPrim s) -> IO (Ptr PangoLayout)

pangoLayoutThaw ::
	PrimMonad m => PangoLayout -> m (PangoLayoutPrim (PrimState m))
pangoLayoutThaw (PangoLayout fpl) = unsafeIOToPrim
	$ withForeignPtr fpl \ppl -> mkPangoLayoutPrim =<< c_pango_layout_thaw ppl

foreign import ccall "pango_layout_copy" c_pango_layout_thaw ::
	Ptr PangoLayout -> IO (Ptr (PangoLayoutPrim s))
