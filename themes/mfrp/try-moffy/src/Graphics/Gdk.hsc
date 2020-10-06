{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Control.Exception
import Data.Bits
import Data.Bool
import Data.Word
import Data.Int

import Graphics.CairoType

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

newtype GdkEvent = GdkEvent (Ptr GdkEvent) deriving Show

newtype GdkEventType = GdkEventType #{type GdkEventType} deriving Show

#enum GdkEventType, GdkEventType, \
	GDK_NOTHING, GDK_DELETE, GDK_DESTROY, GDK_EXPOSE, GDK_MOTION_NOTIFY, \
	GDK_BUTTON_PRESS, GDK_2BUTTON_PRESS, GDK_DOUBLE_BUTTON_PRESS, \
	GDK_3BUTTON_PRESS, GDK_TRIPLE_BUTTON_PRESS, GDK_BUTTON_RELEASE, \
	GDK_KEY_PRESS, GDK_KEY_RELEASE, GDK_ENTER_NOTIFY, GDK_LEAVE_NOTIFY, \
	GDK_FOCUS_CHANGE, GDK_CONFIGURE, GDK_MAP, GDK_UNMAP, GDK_PROPERTY_NOTIFY, \
	GDK_SELECTION_CLEAR, GDK_SELECTION_REQUEST, GDK_SELECTION_NOTIFY, \
	GDK_PROXIMITY_IN, GDK_PROXIMITY_OUT, GDK_DRAG_ENTER, GDK_DRAG_LEAVE, \
	GDK_DRAG_MOTION, GDK_DRAG_STATUS, GDK_DROP_START, GDK_DROP_FINISHED, \
	GDK_CLIENT_EVENT, GDK_VISIBILITY_NOTIFY, GDK_SCROLL, GDK_WINDOW_STATE, \
	GDK_SETTING, GDK_OWNER_CHANGE, GDK_GRAB_BROKEN

gdkEventType :: GdkEvent -> IO GdkEventType
gdkEventType (GdkEvent p) = GdkEventType <$> #{peek GdkEvent, type} p

foreign import ccall "gdk_event_get" c_gdk_event_get :: IO (Ptr GdkEvent)
foreign import ccall "gdk_events_pending" c_gdk_event_pending :: IO #type gboolean

gdkEventGet :: IO GdkEvent
gdkEventGet = GdkEvent <$> c_gdk_event_get

gdkWithEvent :: (Maybe GdkEvent -> IO a) -> IO a
gdkWithEvent f = bracket
	c_gdk_event_get
	(\p -> bool (c_gdk_event_free p) (pure ()) $ p == nullPtr)
	(\p -> f $ bool (Just $ GdkEvent p) Nothing $ p == nullPtr)

foreign import ccall "gdk_event_free" c_gdk_event_free :: Ptr GdkEvent -> IO ()

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
