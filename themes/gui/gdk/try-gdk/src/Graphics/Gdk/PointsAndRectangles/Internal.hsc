{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.PointsAndRectangles.Internal (
	-- * GDK RECTANGLE
	GdkRectangle(..),
	pattern GdkRectangle,
	gdkRectangleX, gdkRectangleY, gdkRectangleWidth, gdkRectangleHeight,

	-- * GDK RECTANGLE PRIM
	GdkRectanglePrim(..), GdkRectangleIO, GdkRectangleST,
	gdkRectangleNew, gdkRectangleFreeze, gdkRectangleThaw, gdkRectangleCopy,

	-- * FUNCTION
	gdkRectangleIntersect, gdkRectangleUnion, gdkRectangleEqual,
	) where

import Control.Monad.Primitive
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Struct
import Data.Int
import System.IO.Unsafe
import System.GLib.Bool

#include <gdk/gdk.h>

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

gdkRectangleIntersect :: PrimMonad m =>
	GdkRectangle -> GdkRectangle -> GdkRectanglePrim (PrimState m) -> m Bool
gdkRectangleIntersect
	(GdkRectangle_ fs) (GdkRectangle_ ft) (GdkRectanglePrim fd) =
	unsafeIOToPrim $ gbooleanToBool
		<$> withForeignPtr fs \ps -> withForeignPtr ft \pt ->
			withForeignPtr fd $ c_gdk_rectangle_intersect ps pt

foreign import ccall "gdk_rectangle_intersect" c_gdk_rectangle_intersect ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> Ptr GdkRectangle -> IO #type gboolean

gdkRectangleUnion :: PrimMonad m =>
	GdkRectangle -> GdkRectangle -> GdkRectanglePrim (PrimState m) -> m ()
gdkRectangleUnion (GdkRectangle_ fs) (GdkRectangle_ ft) (GdkRectanglePrim fd) =
	unsafeIOToPrim $ withForeignPtr fs \ps -> withForeignPtr ft \pt ->
		withForeignPtr fd $ c_gdk_rectangle_union ps pt

foreign import ccall "gdk_rectangle_union" c_gdk_rectangle_union ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> Ptr GdkRectangle -> IO ()

gdkRectangleEqual :: GdkRectangle -> GdkRectangle -> Bool
gdkRectangleEqual (GdkRectangle_ fr1) (GdkRectangle_ fr2) = unsafePerformIO
	$ withForeignPtr fr1 \pr1 -> withForeignPtr fr2 \pr2 ->
		gbooleanToBool <$> c_gdk_rectangle_equal pr1 pr2

foreign import ccall "gdk_rectangle_equal" c_gdk_rectangle_equal ::
	Ptr GdkRectangle -> Ptr GdkRectangle -> IO #type gboolean
