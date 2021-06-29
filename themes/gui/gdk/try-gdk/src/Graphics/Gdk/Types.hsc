{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.C

#include <gdk/gdk.h>

newtype GdkWindowAutoUnref = GdkWindowAutoUnref (ForeignPtr GdkWindowNeedUnref) deriving Show
data GdkWindowNeedUnref

mkGdkWindowAutoUnref :: Ptr GdkWindowNeedUnref -> IO GdkWindowAutoUnref
mkGdkWindowAutoUnref p = GdkWindowAutoUnref <$> newForeignPtr p (c_g_object_unref p)

newtype GdkDrawingContext = GdkDrawingContext (Ptr GdkDrawingContext) deriving Show

-- newtype GdkRectangle = GdkRectangle (Ptr GdkRectangle) deriving Show

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newForeignCString :: String -> IO ForeignCString
newForeignCString s = do
	cs <- newCString s
	newForeignPtr cs (free cs)

type ForeignCString = ForeignPtr CChar

newtype GdkDisplayManager = GdkDisplayManager (Ptr GdkDisplayManager) deriving Show

newtype GdkMonitor = GdkMonitor (Ptr GdkMonitor) deriving Show
