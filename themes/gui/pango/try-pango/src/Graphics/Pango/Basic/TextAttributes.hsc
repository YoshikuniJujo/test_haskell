{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.TextAttributes where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

newtype PangoAttrList = PangoAttrList (ForeignPtr PangoAttrList) deriving Show

mkPangoAttrList :: Ptr PangoAttrList -> IO PangoAttrList
mkPangoAttrList p = PangoAttrList <$> newForeignPtr p (c_pango_attr_list_unref p)

foreign import ccall "pango_attr_list_unref" c_pango_attr_list_unref ::
	Ptr PangoAttrList -> IO ()
