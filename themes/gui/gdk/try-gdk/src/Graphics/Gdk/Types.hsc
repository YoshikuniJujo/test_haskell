{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Types where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Data.Word
import Data.Int

import Graphics.Gdk.Values

#include <gdk/gdk.h>

newtype GdkWindow = GdkWindow (ForeignPtr GdkWindow) deriving Show
newtype GdkWindowAttr = GdkWindowAttr (Ptr GdkWindowAttr) deriving Show

gdkWindowAttrSetEventMask :: GdkWindowAttr -> [GdkEventMask] -> IO ()
gdkWindowAttrSetEventMask (GdkWindowAttr attr) ems =
	#{poke GdkWindowAttr, event_mask} attr $ mergeGdkEventMask ems

gdkWindowAttrSetWindowType :: GdkWindowAttr -> GdkWindowType -> IO ()
gdkWindowAttrSetWindowType (GdkWindowAttr attr) (GdkWindowType wt) = #{poke GdkWindowAttr, window_type} attr wt

gdkWindowAttrSetX, gdkWindowAttrSetY :: GdkWindowAttr -> #{type gint} -> IO ()
gdkWindowAttrSetX (GdkWindowAttr attr) = #{poke GdkWindowAttr, x} attr
gdkWindowAttrSetY (GdkWindowAttr attr) = #{poke GdkWindowAttr, y} attr

gdkWindowAttrSetWidth, gdkWindowAttrSetHeight :: GdkWindowAttr -> #{type gint} -> IO ()
gdkWindowAttrSetWidth (GdkWindowAttr attr) = #{poke GdkWindowAttr, width} attr
gdkWindowAttrSetHeight (GdkWindowAttr attr) = #{poke GdkWindowAttr, height} attr

gdkWindowAttrSetWClass :: GdkWindowAttr -> GdkWindowWindowClass -> IO ()
gdkWindowAttrSetWClass (GdkWindowAttr attr) (GdkWindowWindowClass c) = #{poke GdkWindowAttr, wclass} attr c

instance Storable GdkWindowAttr where
	sizeOf _ = #{size GdkWindowAttr}
	alignment _ = #{alignment GdkWindowAttr}
	peek = pure . GdkWindowAttr
	poke _ _ = pure ()

foreign import ccall "gdk_window_destroy" c_gdk_window_destroy ::
	Ptr GdkWindow -> IO ()

makeGdkWindow :: Ptr GdkWindow -> IO GdkWindow
makeGdkWindow p = GdkWindow <$> newForeignPtr p (free p)

newtype GdkDrawingContext = GdkDrawingContext (Ptr GdkDrawingContext) deriving Show

newtype GdkRectangle = GdkRectangle (Ptr GdkRectangle) deriving Show

data GdkWindowAttr' = GdkWindowAttr' {
	gdkWindowAttrTitle :: Maybe String,
	gdkWindowAttrEventMask :: GdkEventMask,
	gdkWindowAttrX, gdkWindowAttrY :: Maybe #{type gint},
	gdkWindowAttrWidth, gdkWindowAttrHeight :: #{type gint},
	gdkWindowAttrWclass :: GdkWindowWindowClass,
	gdkWindowAttrVisual :: Maybe GdkVisual,
	gdkWindowAttrWindowType :: GdkWindowType,
	gdkWindowAttrCursor :: Maybe GdkCursor,
	gdkWindowAttrOverrideRedirect :: Bool,
	gdkWindowAttrTypeHing :: Maybe GdkWindowTypeHint } deriving Show

newtype GdkVisual = GdkVisual (ForeignPtr GdkVisual) deriving Show
newtype GdkCursor = GdkCursor (ForeignPtr GdkCursor) deriving Show

newtype GdkWindowTypeHint = GdkWindowTypeHint #{type GdkWindowTypeHint} deriving Show
