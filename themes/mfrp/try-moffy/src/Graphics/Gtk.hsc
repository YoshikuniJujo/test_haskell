{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gtk (
	-- * Basic
	GtkWidget, gtkInit, gtkMain, gtkMainQuit,
	gtkWidgetSetEvents, gtkWidgetShowAll,
	gtkWidgetQueueDraw,
	-- * Widget
	gtkWindowNew, gtkWindowToplevel, gtkWindowPopup,
	gtkDrawingAreaNew,
	-- * Container
	castWidgetToContainer, gtkContainerAdd,
	-- * Gdk Event Mask
	gdkPointerMotionMask, gdkScrollMask,
	-- * Event General
	Event, Handler, AsPointer, gSignalConnect, gTimeoutAdd,
	-- * Each Event
	-- ** Destroy
	Destroy(..),
	-- ** DeleteEvent
	DeleteEvent(..),
	-- ** KeyEvent
	KeyEvent(..), GdkEventKey, keyval, hardwareKeycode,
	-- ** ButtonEvent
	ButtonEvent(..), GdkEventButton, gdkEventButtonButton, gdkEventButtonX, gdkEventButtonY,
	-- ** ScrollEvent
	ScrollEvent(..), GdkEventScroll,
	-- ** MotionNotifyEvent
	MotionNotifyEvent(..), GdkEventMotion, gdkEventMotionX, gdkEventMotionY,
	-- ** DrawEvent
	DrawEvent(..), CairoT,
	-- * Mutable
	Mutable, allocaMutable, peekMutable, pokeMutable,
	-- * Others
	gtkWidgetGetAllocatedWidth, gtkWidgetGetAllocatedHeight
	) where

#include <gtk/gtk.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Int

import Graphics.Gtk.CairoType
import Graphics.Gtk.Values
import Graphics.Gtk.AsPointer

newtype GtkWidget = GtkWidget (Ptr GtkWidget) deriving Show
newtype GtkContainer = GtkContainer (Ptr GtkContainer) deriving Show

instance AsPointer GtkWidget where
	asPointer (GtkWidget p) f = f p
	asValue = pure . GtkWidget

newtype Mutable a = Mutable (Ptr a) deriving Show

allocaMutable :: Storable a => (Mutable a -> IO b) -> IO b
allocaMutable f = alloca $ f . Mutable

peekMutable :: Storable a => Mutable a -> IO a
peekMutable (Mutable p) = peek p

pokeMutable :: Storable a => Mutable a -> a -> IO ()
pokeMutable (Mutable p) = poke p

instance AsPointer (Mutable a) where
	asPointer (Mutable p) f = f $ castPtr p
	asValue = pure . Mutable . castPtr

foreign import ccall "gtk_init" c_gtk_init :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
foreign import ccall "gtk_widget_show_all" c_gtk_widget_show_all :: Ptr GtkWidget -> IO ()
foreign import ccall "gtk_main" c_gtk_main :: IO ()
foreign import ccall "gtk_main_quit" c_gtk_main_quit :: IO ()
foreign import ccall "gtk_container_add" c_gtk_container_add :: Ptr GtkContainer -> Ptr GtkWidget -> IO ()
foreign import capi "GTK_CONTAINER" c_GTK_CONTAINER :: Ptr GtkWidget -> Ptr GtkContainer

foreign import ccall "gtk_window_new" c_gtk_window_new :: #{type GtkWindowType} -> IO (Ptr GtkWidget)
foreign import ccall "gtk_drawing_area_new" c_gtk_drawing_area_new :: IO (Ptr GtkWidget)

foreign import ccall "gtk_widget_queue_draw" c_gtk_widget_queue_draw :: Ptr GtkWidget -> IO ()

foreign import ccall "g_timeout_add" c_g_timeout_add ::
	#{type guint} -> FunPtr (Ptr a -> IO #{type gboolean}) -> Ptr a -> IO #type guint

gTimeoutAdd :: AsPointer a => #{type guint} -> (a -> IO Bool) -> a -> IO #type guint
gTimeoutAdd t f x = do
	fp <- g_callback_timeout \x' -> (boolToGBoolean <$>) . f =<< asValue x'
	asPointer x $ c_g_timeout_add t fp

gtkWidgetQueueDraw :: GtkWidget -> IO ()
gtkWidgetQueueDraw (GtkWidget w) = c_gtk_widget_queue_draw w

castWidgetToContainer :: GtkWidget -> GtkContainer
castWidgetToContainer (GtkWidget w) = GtkContainer $ c_GTK_CONTAINER w

gtkContainerAdd :: GtkContainer -> GtkWidget -> IO ()
gtkContainerAdd (GtkContainer c) (GtkWidget w) = c_gtk_container_add c w

gtkMainQuit :: IO ()
gtkMainQuit = c_gtk_main_quit

{-
type Handler e a = Ptr GtkWidget -> Ptr e -> Ptr a -> IO ()
data GdkEventKey = GdkEventKey (Ptr GdkEventKey)
-}

boolToGBoolean :: Bool -> #type gboolean
boolToGBoolean False = #const FALSE
boolToGBoolean True = #const TRUE

class Event e where
	type Handler e a
	type CHandler e a
	eventName :: e -> String
	handlerToCHandler :: AsPointer a => Handler e a -> CHandler e a
	g_callback :: CHandler e a -> IO (FunPtr (CHandler e a))

data Destroy = Destroy deriving Show
instance Event Destroy where
	type Handler Destroy a = IO ()
	type CHandler Destroy a = IO ()
	handlerToCHandler = id
	eventName Destroy = "destroy"
	g_callback = g_callback0

data DeleteEvent = DeleteEvent deriving Show
newtype GdkEventDelete = GdkEventDelete (Ptr GdkEventDelete) deriving Show
instance Event DeleteEvent where
	type Handler DeleteEvent a = GtkWidget -> GdkEventDelete -> a -> IO Bool
	type CHandler DeleteEvent a = GtkWidget -> GdkEventDelete -> Ptr a -> IO #type gboolean
	eventName DeleteEvent = "delete-event"
	handlerToCHandler = handlerToCHandlerDelete
	g_callback = g_callback_delete

handlerToCHandlerDelete :: AsPointer a => Handler DeleteEvent a -> CHandler DeleteEvent a
handlerToCHandlerDelete h w e px = do
	x <- asValue px
	boolToGBoolean <$> h w e x

data KeyEvent = KeyPressEvent | KeyReleaseEvent deriving Show
instance Event KeyEvent where
	type Handler KeyEvent a = GtkWidget -> GdkEventKey -> a -> IO Bool
	type CHandler KeyEvent a = GtkWidget -> GdkEventKey -> Ptr a -> IO #type gboolean
	eventName KeyPressEvent = "key-press-event"
	eventName KeyReleaseEvent = "key-release-event"
	handlerToCHandler = handlerToCHandlerKey
	g_callback = g_callback_key
newtype GdkEventKey = GdkEventKey (Ptr GdkEventKey) deriving Show

handlerToCHandlerKey :: AsPointer a => Handler KeyEvent a -> CHandler KeyEvent a
handlerToCHandlerKey h w e px = do
	x <- asValue px
	boolToGBoolean <$> h w e x

keyval :: GdkEventKey -> IO #type guint
keyval (GdkEventKey p) = c_keyval p

c_keyval :: Ptr GdkEventKey -> IO #type guint
c_keyval = #peek GdkEventKey, keyval

hardwareKeycode :: GdkEventKey -> IO #type guint16
hardwareKeycode (GdkEventKey p) = c_hardware_keycode p

c_hardware_keycode :: Ptr GdkEventKey -> IO #type guint16
c_hardware_keycode = #peek GdkEventKey, hardware_keycode

data ButtonEvent = ButtonPressEvent | ButtonReleaseEvent deriving Show
newtype GdkEventButton = GdkEventButton (Ptr GdkEventButton) deriving Show
instance Event ButtonEvent where
	type Handler ButtonEvent a = GtkWidget -> GdkEventButton -> a -> IO Bool
	type CHandler ButtonEvent a = GtkWidget -> GdkEventButton -> Ptr a -> IO #type gboolean
	eventName ButtonPressEvent = "button-press-event"
	eventName ButtonReleaseEvent = "button-release-event"
	handlerToCHandler = handlerToCHandlerButton
	g_callback = g_callback_button

data ScrollEvent = ScrollEvent deriving Show
newtype GdkEventScroll = GdkEventScroll (Ptr GdkEventScroll) deriving Show
instance Event ScrollEvent where
	type Handler ScrollEvent a = GtkWidget -> GdkEventScroll -> a -> IO Bool
	type CHandler ScrollEvent a = GtkWidget -> GdkEventScroll -> Ptr a -> IO #type gboolean
	eventName ScrollEvent = "scroll-event"
	handlerToCHandler = handlerToCHandlerEvent
	g_callback = g_callback_scroll

gdkEventButtonButton :: GdkEventButton -> IO #type guint
gdkEventButtonButton (GdkEventButton e) = c_GdkEventButton_button e

c_GdkEventButton_button :: Ptr GdkEventButton -> IO #type guint
c_GdkEventButton_button = #peek GdkEventButton, button

gdkEventButtonX, gdkEventButtonY :: GdkEventButton -> IO #type gdouble
gdkEventButtonX (GdkEventButton e) = c_GdkEventButton_x e
gdkEventButtonY (GdkEventButton e) = c_GdkEventButton_y e

c_GdkEventButton_x, c_GdkEventButton_y :: Ptr GdkEventButton -> IO #type gdouble
c_GdkEventButton_x = #peek GdkEventButton, x
c_GdkEventButton_y = #peek GdkEventButton, y

handlerToCHandlerButton :: AsPointer a => Handler ButtonEvent a -> CHandler ButtonEvent a
handlerToCHandlerButton h w e px = do
	x <- asValue px
	boolToGBoolean <$> h w e x

handlerToCHandlerEvent :: AsPointer a => Handler ScrollEvent a -> CHandler ScrollEvent a
handlerToCHandlerEvent h w e px = do
	x <- asValue px
	boolToGBoolean <$> h w e x

data MotionNotifyEvent = MotionNotifyEvent deriving Show
newtype GdkEventMotion = GdkEventMotion (Ptr GdkEventMotion) deriving Show
instance Event MotionNotifyEvent where
	type Handler MotionNotifyEvent a = GtkWidget -> GdkEventMotion -> a -> IO Bool
	type CHandler MotionNotifyEvent a = GtkWidget -> GdkEventMotion -> Ptr a -> IO #type gboolean
	eventName MotionNotifyEvent = "motion-notify-event"
	handlerToCHandler = handlerToCHandlerMotion
	g_callback = g_callback_motion

gdkEventMotionX, gdkEventMotionY :: GdkEventMotion -> IO #type gdouble
gdkEventMotionX (GdkEventMotion p) = c_GdkEventMotion_x p
gdkEventMotionY (GdkEventMotion p) = c_GdkEventMotion_y p

c_GdkEventMotion_x, c_GdkEventMotion_y :: Ptr GdkEventMotion -> IO #type gdouble
c_GdkEventMotion_x = #peek GdkEventMotion, x
c_GdkEventMotion_y = #peek GdkEventMotion, y

handlerToCHandlerMotion :: AsPointer a => Handler MotionNotifyEvent a -> CHandler MotionNotifyEvent a
handlerToCHandlerMotion h w e px = do
	x <- asValue px
	boolToGBoolean <$> h w e x

foreign import capi "gtk/gtk.h g_signal_connect" c_g_signal_connect ::
	Ptr GtkWidget -> CString -> FunPtr () -> Ptr a -> IO ()

data DrawEvent = DrawEvent deriving Show
instance Event DrawEvent where
	type Handler DrawEvent a = GtkWidget -> CairoT -> a -> IO Bool
	type CHandler DrawEvent a = GtkWidget -> CairoT -> Ptr a -> IO #type gboolean
	eventName DrawEvent = "draw"
	handlerToCHandler = handlerToCHandlerDraw
	g_callback = g_callback_draw

handlerToCHandlerDraw :: AsPointer a => Handler DrawEvent a -> CHandler DrawEvent a
handlerToCHandlerDraw h w e px = do
	x <- asValue px
	boolToGBoolean <$> h w e x

-- foreign import ccall "wrapper" g_callback :: Handler e a -> IO (FunPtr (Handler e a))
foreign import ccall "wrapper" g_callback0 :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wrapper" g_callback_key ::
	(GtkWidget -> GdkEventKey -> Ptr a -> IO #{type gboolean}) -> IO (FunPtr (GtkWidget -> GdkEventKey -> Ptr a -> IO #{type gboolean}))
foreign import ccall "wrapper" g_callback_button ::
	(GtkWidget -> GdkEventButton -> Ptr a -> IO #{type gboolean}) -> IO (FunPtr (GtkWidget -> GdkEventButton -> Ptr a -> IO #{type gboolean}))
foreign import ccall "wrapper" g_callback_delete :: CHandler DeleteEvent a -> IO (FunPtr (CHandler DeleteEvent a))
foreign import ccall "wrapper" g_callback_motion :: CHandler MotionNotifyEvent a -> IO (FunPtr (CHandler MotionNotifyEvent a))
foreign import ccall "wrapper" g_callback_draw :: CHandler DrawEvent a -> IO (FunPtr (CHandler DrawEvent a))
foreign import ccall "wrapper" g_callback_timeout ::
	(Ptr a -> IO #{type gboolean}) -> IO (FunPtr (Ptr a -> IO #{type gboolean}))
foreign import ccall "wrapper" g_callback_scroll ::
	(GtkWidget -> GdkEventScroll -> Ptr a -> IO #{type gboolean}) -> IO (FunPtr (GtkWidget -> GdkEventScroll -> Ptr a -> IO #{type gboolean}))

-- foreign import ccall "hello_main" c_hello_main :: IO ()

gSignalConnect :: forall e a . (Event e, AsPointer a) => GtkWidget -> e -> Handler e a -> a -> IO ()
gSignalConnect (GtkWidget pw) e (handlerToCHandler @e @a -> h) x = do
	cs <- newCString $ eventName e
	cb <- castFunPtr <$> g_callback @e @a h
	asPointer x $ c_g_signal_connect pw cs cb

gtkInit :: [String] -> IO [String]
gtkInit as = allocaArray (length as) \arr -> do
	cas <- newCString `mapM` as
	pokeArray arr cas
	(n', arr') <- alloca \pn -> do
		poke pn . fromIntegral $ length as
		arr' <- alloca \parr -> do
			poke parr arr
			c_gtk_init pn parr
			peek parr
		(, arr') <$> peek pn
	(peekCString `mapM`) =<< peekArray (fromIntegral n') arr'

gtkWindowNew :: GtkWindowType -> IO GtkWidget
gtkWindowNew (GtkWindowType wt) = GtkWidget <$> c_gtk_window_new wt

gtkDrawingAreaNew :: IO GtkWidget
gtkDrawingAreaNew = GtkWidget <$> c_gtk_drawing_area_new

gtkWidgetShowAll :: GtkWidget -> IO ()
gtkWidgetShowAll (GtkWidget pw) = c_gtk_widget_show_all pw

gtkMain :: IO ()
gtkMain = c_gtk_main

foreign import ccall "gtk_widget_set_events" c_gtk_widget_set_events :: Ptr GtkWidget -> #{type gint} -> IO ()

gtkWidgetSetEvents :: GtkWidget -> [GdkEventMask] -> IO ()
gtkWidgetSetEvents (GtkWidget w) ms = c_gtk_widget_set_events w ms'
	where GdkEventMask ms' = unifyGdkEventMask ms

foreign import ccall "gtk_widget_get_allocated_width" c_gtk_widget_get_allocated_width :: Ptr GtkWidget -> IO #{type int}
foreign import ccall "gtk_widget_get_allocated_height" c_gtk_widget_get_allocated_height :: Ptr GtkWidget -> IO #{type int}

gtkWidgetGetAllocatedWidth, gtkWidgetGetAllocatedHeight :: GtkWidget -> IO #{type int}
gtkWidgetGetAllocatedWidth (GtkWidget w) = c_gtk_widget_get_allocated_width w
gtkWidgetGetAllocatedHeight (GtkWidget w) = c_gtk_widget_get_allocated_height w
