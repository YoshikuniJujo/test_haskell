{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.GdkOld where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Exception
import Data.Bits
import Data.Word
import Data.Int

import Graphics.CairoType
-- import Graphics.Gdk.Event

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
#enum GdkWindowType, GdkWindowType, GDK_WINDOW_ROOT, GDK_WINDOW_TOPLEVEL, \
	GDK_WINDOW_CHILD

newtype GdkWindowWindowClass = GdkWindowWindowClass #{type GdkWindowWindowClass} deriving Show
#enum GdkWindowWindowClass, GdkWindowWindowClass, GDK_INPUT_OUTPUT, GDK_INPUT_ONLY

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

newtype GdkDrawingContext = GdkDrawingContext (Ptr GdkDrawingContext) deriving Show

data CairoRectangleIntT = CairoRectangleIntT {
	cairoRectangleIntTX, cairoRectangleIntTY :: #{type int},
	cairoRectangleIntTWidth, cairoRectangleIntTHeight :: #{type int} } deriving Show

instance Storable CairoRectangleIntT where
	sizeOf _ = #size cairo_rectangle_int_t
	alignment _ = #alignment cairo_rectangle_int_t
	peek pr = do
		x <- #{peek cairo_rectangle_int_t, x} pr
		y <- #{peek cairo_rectangle_int_t, y} pr
		w <- #{peek cairo_rectangle_int_t, width} pr
		h <- #{peek cairo_rectangle_int_t, height} pr
		pure $ CairoRectangleIntT x y w h
	poke pr (CairoRectangleIntT x y w h) = do
		#{poke cairo_rectangle_int_t, x} pr x
		#{poke cairo_rectangle_int_t, y} pr y
		#{poke cairo_rectangle_int_t, width} pr w
		#{poke cairo_rectangle_int_t, height} pr h

foreign import ccall "cairo_region_create_rectangle" c_cairo_region_create_rectangle ::
	Ptr CairoRectangleIntT -> IO (Ptr CairoRegionT)

newtype CairoRegionT = CairoRegionT (Ptr CairoRegionT) deriving Show

{-
cairoRegionCreateRectangle :: CairoRectangleIntT -> IO CairoRegionT
cairoRegionCreateRectangle r = (CairoRegionT <$>) $ alloca \p ->
	poke p r *> c_cairo_region_create_rectangle p
	-}

foreign import ccall "cairo_region_destroy" c_cairo_region_destroy ::
	Ptr CairoRegionT -> IO ()

cairoRegionWithRectangle :: CairoRectangleIntT -> (CairoRegionT -> IO a) -> IO a
cairoRegionWithRectangle r = bracket
	(alloca \p -> poke p r *> c_cairo_region_create_rectangle p)
	c_cairo_region_destroy . (. CairoRegionT)

foreign import ccall "gdk_window_begin_draw_frame" c_gdk_window_begin_draw_frame ::
	Ptr GdkWindow -> Ptr CairoRegionT -> IO (Ptr GdkDrawingContext)

foreign import ccall "gdk_window_end_draw_frame" c_gdk_window_end_draw_frame ::
	Ptr GdkWindow -> Ptr GdkDrawingContext -> IO ()

foreign import ccall "gdk_drawing_context_get_cairo_context" c_gdk_drawing_context_get_cairo_context ::
	Ptr GdkDrawingContext -> IO (Ptr CairoT)

gdkWindowWithDrawFrame :: GdkWindow -> CairoRegionT -> (GdkDrawingContext -> IO a) -> IO a
gdkWindowWithDrawFrame (GdkWindow w) (CairoRegionT r) = bracket
	(c_gdk_window_begin_draw_frame w r) (c_gdk_window_end_draw_frame w) . (. GdkDrawingContext)

gdkDrawingContextGetCairoContext :: GdkDrawingContext -> IO CairoT
gdkDrawingContextGetCairoContext (GdkDrawingContext c) = CairoT <$> c_gdk_drawing_context_get_cairo_context c

newtype GdkEventMask = GdkEventMask #{type GdkEventMask} deriving Show

#enum GdkEventMask, GdkEventMask, GDK_EXPOSURE_MASK, \
	GDK_BUTTON_PRESS_MASK

mergeGdkEventMask :: [GdkEventMask] -> #{type GdkEventMask}
mergeGdkEventMask [] = 0
mergeGdkEventMask (GdkEventMask em : ems) = em .|. mergeGdkEventMask ems

foreign import ccall "gdk_window_set_events" c_gdk_window_set_events :: Ptr GdkWindow -> #{type GdkEventMask} -> IO ()

gdkWindowSetEvents :: GdkWindow -> [GdkEventMask] -> IO ()
gdkWindowSetEvents (GdkWindow p) = c_gdk_window_set_events p . mergeGdkEventMask

foreign import ccall "gdk_window_invalidate_rect" c_gdk_window_invalidate_rect ::
	Ptr GdkWindow -> Ptr GdkRectangle -> #{type gboolean} -> IO ()

gdkWindowInvalidateRect :: GdkWindow -> (#{type int}, #{type int}) -> (#{type int}, #{type int}) -> Bool -> IO ()
gdkWindowInvalidateRect (GdkWindow win) (x, y) (w, h) b = allocaBytes #{size GdkRectangle} \p -> do
	#{poke GdkRectangle, x} p x
	#{poke GdkRectangle, y} p y
	#{poke GdkRectangle, width} p w
	#{poke GdkRectangle, height} p h
	c_gdk_window_invalidate_rect win p $ boolToGboolean b

boolToGboolean :: Bool -> #type gboolean
boolToGboolean False = #const FALSE
boolToGboolean True = #const TRUE

newtype GdkRectangle = GdkRectangle (Ptr GdkRectangle) deriving Show

gdkRectangleSetX, gdkRectangleSetY :: GdkRectangle -> #{type int} -> IO ()
gdkRectangleSetX (GdkRectangle p) = #{poke GdkRectangle, x} p
gdkRectangleSetY (GdkRectangle p) = #{poke GdkRectangle, y} p

gdkRectangleSetWidth, gdkRectangleSetHeight :: GdkRectangle -> #{type int} -> IO ()
gdkRectangleSetWidth (GdkRectangle p) = #{poke GdkRectangle, width} p
gdkRectangleSetHeight (GdkRectangle p) = #{poke GdkRectangle, height} p

foreign import ccall "gdk_window_thaw_updates" c_gdk_window_thaw_updates :: Ptr GdkWindow -> IO ()

gdkWindowThawUpdates :: GdkWindow -> IO ()
gdkWindowThawUpdates (GdkWindow p) = c_gdk_window_thaw_updates p

foreign import ccall "gdk_window_freeze_updates" c_gdk_window_freeze_updates :: Ptr GdkWindow -> IO ()

gdkWindowFreezeUpdates :: GdkWindow -> IO ()
gdkWindowFreezeUpdates (GdkWindow p) = c_gdk_window_freeze_updates p
