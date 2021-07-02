{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent

#include <gdk/gdk.h>

newtype GdkWindowAutoUnref = GdkWindowAutoUnref (ForeignPtr GdkWindowNeedUnref) deriving Show
data GdkWindowNeedUnref

mkGdkWindowAutoUnref :: Ptr GdkWindowNeedUnref -> IO GdkWindowAutoUnref
mkGdkWindowAutoUnref p = GdkWindowAutoUnref <$> newForeignPtr p (c_g_object_unref p)

-- newtype GdkRectangle = GdkRectangle (Ptr GdkRectangle) deriving Show

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newtype GdkDisplayManager = GdkDisplayManager (Ptr GdkDisplayManager) deriving Show

newtype GdkMonitor = GdkMonitor (Ptr GdkMonitor) deriving Show
