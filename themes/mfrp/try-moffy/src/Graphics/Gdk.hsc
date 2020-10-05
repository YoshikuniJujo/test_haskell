{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Data.Bits
import Data.Word
import Data.Int

#include <gdk/gdk.h>
#include <glib.h>

newtype GdkWindow = GdkWindow (Ptr GdkWindow) deriving Show
newtype GdkWindowAttr = GdkWindowAttr (Ptr GdkWindowAttr) deriving Show
newtype GMainLoop = GMainLoop (Ptr GMainLoop) deriving Show

foreign import ccall "gdk_init" c_gdk_init :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

gdkInit :: [String] -> IO [String]
gdkInit as = allocaArray (length as) \arr -> do
	cas <- newCString `mapM` as
	pokeArray arr cas
	(n', arr') <- alloca \pn -> do
		poke pn . fromIntegral $ length as
		arr' <- alloca \parr -> do
			poke parr arr
			c_gdk_init pn parr
			peek parr
		(, arr') <$> peek pn
	(peekCString `mapM`) =<< peekArray (fromIntegral n') arr'

newtype GdkWindowType = GdkWindowType #{type GdkWindowType} deriving Show
#enum GdkWindowType, GdkWindowType, GDK_WINDOW_TOPLEVEL

newtype GdkWindowWindowClass = GdkWindowWindowClass #{type GdkWindowWindowClass} deriving Show
#enum GdkWindowWindowClass, GdkWindowWindowClass, GDK_INPUT_OUTPUT, GDK_INPUT_ONLY

gdkWindowAttrSetWindowType :: GdkWindowAttr -> GdkWindowType -> IO ()
gdkWindowAttrSetWindowType (GdkWindowAttr attr) (GdkWindowType wt) = #{poke GdkWindowAttr, window_type} attr wt

gdkWindowAttrSetWidth, gdkWindowAttrSetHeight :: GdkWindowAttr -> #{type gint} -> IO ()
gdkWindowAttrSetWidth (GdkWindowAttr attr) = #{poke GdkWindowAttr, width} attr
gdkWindowAttrSetHeight (GdkWindowAttr attr) = #{poke GdkWindowAttr, height} attr

gdkWindowAttrSetWClass :: GdkWindowAttr -> GdkWindowWindowClass -> IO ()
gdkWindowAttrSetWClass (GdkWindowAttr attr) (GdkWindowWindowClass c) = #{poke GdkWindowAttr, wclass} attr c

newtype GdkWindowAttributesType = GdkWindowAttributesType #{type GdkWindowAttributesType} deriving Show
#enum GdkWindowAttributesType, GdkWindowAttributesType, \
	GDK_WA_TITLE, GDK_WA_X, GDK_WA_Y, GDK_WA_CURSOR, GDK_WA_VISUAL, \
	GDK_WA_WMCLASS, GDK_WA_NOREDIR, GDK_WA_TYPE_HINT

foreign import ccall "gdk_window_new" c_gdk_window_new ::
	Ptr GdkWindow -> Ptr GdkWindowAttr -> #{type GdkWindowAttributesType} -> IO (Ptr GdkWindow)

mergeGdkWindowAttributesType :: [GdkWindowAttributesType] -> #{type GdkWindowAttributesType}
mergeGdkWindowAttributesType [] = 0
mergeGdkWindowAttributesType (GdkWindowAttributesType at : ats) =
	at .|. mergeGdkWindowAttributesType ats

gdkWindowNew :: Maybe GdkWindow -> GdkWindowAttr -> [GdkWindowAttributesType] -> IO GdkWindow
gdkWindowNew mp (GdkWindowAttr attr) am = GdkWindow <$> c_gdk_window_new
	(maybe nullPtr (\(GdkWindow p) -> p) mp) attr (mergeGdkWindowAttributesType am)

foreign import ccall "gdk_window_show" c_gdk_window_show :: Ptr GdkWindow -> IO ()

gdkWindowShow :: GdkWindow -> IO ()
gdkWindowShow (GdkWindow w) = c_gdk_window_show w

{-
foreign import capi "g_main_new" c_g_main_new :: #{type gboolean} -> IO (Ptr GMainLoop)

boolToGBoolean :: Bool -> #{type gboolean}
boolToGBoolean False = #{const FALSE}
boolToGBoolean True = #{const TRUE}
-}

instance Storable GdkWindowAttr where
	sizeOf _ = #{size GdkWindowAttr}
	alignment _ = #{alignment GdkWindowAttr}
	peek = pure . GdkWindowAttr
	poke _ _ = pure ()
