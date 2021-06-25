{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Types where

import Control.Monad.Primitive
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Foreign.C.Struct

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

struct "GdkRectangle" #{size GdkRectangle}
	[	("x", ''CInt, [| #{peek GdkRectangle, x} |],
			[| #{poke GdkRectangle, x} |]),
		("y", ''CInt, [| #{peek GdkRectangle, y} |],
			[| #{poke GdkRectangle, y} |]),
		("width", ''CInt, [| #{peek GdkRectangle, width} |],
			[| #{poke GdkRectangle, width} |]),
		("height", ''CInt, [| #{peek GdkRectangle, height} |],
			[| #{poke GdkRectangle, height} |])	]
	[''Show, ''Eq]

c_gdk_rectangle_copy :: Ptr GdkRectangle -> IO (Ptr GdkRectangle)
c_gdk_rectangle_copy s = do
	d <- mallocBytes #{size GdkRectangle}
	#{poke GdkRectangle, x} d =<< (#{peek GdkRectangle, x} s :: IO CInt)
	#{poke GdkRectangle, y} d =<< (#{peek GdkRectangle, y} s :: IO CInt)
	#{poke GdkRectangle, width} d =<< (#{peek GdkRectangle, width} s :: IO CInt)
	#{poke GdkRectangle, height} d =<< (#{peek GdkRectangle, height} s :: IO CInt)
	pure d

c_gdk_rectangle_free :: Ptr GdkRectangle -> IO ()
c_gdk_rectangle_free p = free p

structPrim "GdkRectangle" 'c_gdk_rectangle_copy 'c_gdk_rectangle_free [''Show]

gdkRectangleNew :: PrimMonad m => m (GdkRectanglePrim (PrimState m))
gdkRectangleNew = GdkRectanglePrim <$> unsafeIOToPrim do
	p <- mallocBytes #{size GdkRectangle}
	newForeignPtr p $ free p

newtype GdkDeviceTool = GdkDeviceTool (ForeignPtr GdkDeviceTool) deriving Show
