{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Types where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent

newtype GdkWindow = GdkWindow (ForeignPtr GdkWindow) deriving Show
newtype GdkWindowAttr = GdkWindowAttr (Ptr GdkWindowAttr) deriving Show

foreign import ccall "gdk_window_destroy" c_gdk_window_destroy ::
	Ptr GdkWindow -> IO ()

makeGdkWindow :: Ptr GdkWindow -> IO GdkWindow
makeGdkWindow p = GdkWindow <$> newForeignPtr p (free p)

newtype CairoRegionT = CairoRegionT (Ptr CairoRegionT) deriving Show

newtype GdkDrawingContext = GdkDrawingContext (Ptr GdkDrawingContext) deriving Show
