{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.Rendering where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Data.Word

import Graphics.Pango.Types
import Graphics.Pango.Values

#include <pango/pango.h>

newtype PangoContext = PangoContext (ForeignPtr PangoContext) deriving Show

mkPangoContext :: Ptr PangoContext -> IO PangoContext
mkPangoContext p = PangoContext <$> newForeignPtr p (c_g_object_unref p)

class PangoContextSetting s where
	pangoContextSet :: PangoContext -> s -> IO ()
	pangoContextGet :: PangoContext -> IO s

newtype BaseGravity = BaseGravity { getBaseGravity :: PangoGravity } deriving Show

instance PangoContextSetting BaseGravity where
	pangoContextSet c = pangoContextSetBaseGravity c . getBaseGravity
	pangoContextGet c = BaseGravity <$> pangoContextGetBaseGravity c

pangoContextSetBaseGravity :: PangoContext -> PangoGravity -> IO ()
pangoContextSetBaseGravity (PangoContext fc) (PangoGravity g) =
	withForeignPtr fc \pc -> c_pango_context_set_base_gravity pc g

pangoContextGetBaseGravity :: PangoContext -> IO PangoGravity
pangoContextGetBaseGravity (PangoContext fc) =
	PangoGravity <$> withForeignPtr fc c_pango_context_get_base_gravity

foreign import ccall "pango_context_set_base_gravity"
	c_pango_context_set_base_gravity ::
	Ptr PangoContext -> #{type PangoGravity} -> IO ()

foreign import ccall "pango_context_get_base_gravity"
	c_pango_context_get_base_gravity ::
	Ptr PangoContext -> IO #{type PangoGravity}
