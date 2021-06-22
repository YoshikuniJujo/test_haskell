{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Types where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Int

#include <gdk/gdk.h>

newtype GdkWindow = GdkWindow (Ptr GdkWindow) deriving Show

newtype GdkWindowAutoUnref = GdkWindowAutoUnref (ForeignPtr GdkWindowNeedUnref) deriving Show
data GdkWindowNeedUnref

mkGdkWindowAutoUnref :: Ptr GdkWindowNeedUnref -> IO GdkWindowAutoUnref
mkGdkWindowAutoUnref p = GdkWindowAutoUnref <$> newForeignPtr p (c_g_object_unref p)

withGdkWindowAutoUnref :: GdkWindowAutoUnref -> (GdkWindow -> IO a) -> IO a
withGdkWindowAutoUnref (GdkWindowAutoUnref fwnu) f =
	withForeignPtr fwnu \pwnu -> f (GdkWindow $ castPtr pwnu)

newtype GdkDrawingContext = GdkDrawingContext (Ptr GdkDrawingContext) deriving Show

-- newtype GdkRectangle = GdkRectangle (Ptr GdkRectangle) deriving Show

newtype GdkVisual = GdkVisual (Ptr GdkVisual) deriving Show

newtype GdkCursor = GdkCursor (ForeignPtr GdkCursor) deriving Show
newtype GdkCursorRef = GdkCursorRef (Ptr GdkCursor) deriving Show

mkGdkCursor :: Ptr GdkCursor -> IO GdkCursor
mkGdkCursor p = GdkCursor <$> newForeignPtr p (c_g_object_unref p)

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()

newForeignCString :: String -> IO ForeignCString
newForeignCString s = do
	cs <- newCString s
	newForeignPtr cs (free cs)

type ForeignCString = ForeignPtr CChar

newtype GdkDisplay = GdkDisplay (Ptr GdkDisplay) deriving Show

newtype GdkScreen = GdkScreen (Ptr GdkScreen) deriving Show

newtype GdkDisplayManager = GdkDisplayManager (Ptr GdkDisplayManager) deriving Show

newtype GdkDevice = GdkDevice (ForeignPtr GdkDevice) deriving Show

newtype GdkMonitor = GdkMonitor (Ptr GdkMonitor) deriving Show

data GdkRectangle = GdkRectangle {
	gdkRectangleX, gdkRectangleY :: #{type int},
	gdkRectangleWidth, gdkRectangleHeight :: #{type int}
	} deriving Show

instance Storable GdkRectangle where
	sizeOf _ = #{size GdkRectangle}
	alignment _ = #{alignment GdkRectangle}
	peek p = GdkRectangle
		<$> #{peek GdkRectangle, x} p <*> #{peek GdkRectangle, y} p
		<*> #{peek GdkRectangle, width} p <*> #{peek GdkRectangle, height} p
	poke p gp = do
		#{poke GdkRectangle, x} p $ gdkRectangleX gp
		#{poke GdkRectangle, y} p $ gdkRectangleY gp
		#{poke GdkRectangle, width} p $ gdkRectangleWidth gp
		#{poke GdkRectangle, height} p $ gdkRectangleHeight gp

newtype GdkDeviceTool = GdkDeviceTool (ForeignPtr GdkDeviceTool) deriving Show
