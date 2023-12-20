{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Signal where

import Foreign.Ptr
import Foreign.C.String
import Control.Monad.ST
import Data.String
import Data.CairoContext
import Stopgap.Data.Ptr
import Stopgap.System.GLib.Object qualified as G.Object
import Stopgap.System.GLib.Callback qualified as G.Callback
import Stopgap.Graphics.UI.Gdk.Event.Button qualified as Gdk.Event.Button
import Stopgap.Graphics.UI.Gdk.Event.Motion qualified as Gdk.Event.Motion

newtype Signal = Signal String deriving Show

instance IsString Signal where fromString = Signal

connect_ab :: (G.Object.IsO a, IsPtr b) =>
	a -> Signal -> (a -> b -> IO ()) -> b -> IO ()
connect_ab o (Signal sig) h ud = withCString sig \csig -> do
	ch <- G.Callback.c_ab h
	c_g_signal_connect (toPtr o) csig ch (toPtr ud)

connect_ab_bool :: (G.Object.IsO a, IsPtr b) =>
	a -> Signal -> (a -> b -> IO Bool) -> b -> IO ()
connect_ab_bool o (Signal sig) h ud = withCString sig \csig -> do
	ch <- G.Callback.c_ab_bool h
	c_g_signal_connect (toPtr o) csig ch (toPtr ud)

connect_void_void :: G.Object.IsO a => a -> Signal -> IO () -> Null -> IO ()
connect_void_void o (Signal sig) h Null = withCString sig \csig -> do
	ch <- G.Callback.c_void_void h
	c_g_signal_connect (toPtr o) csig ch nullPtr

connect_self_cairo_ud :: (G.Object.IsO a, IsPtr b) =>
	a -> Signal -> (a -> CairoT r RealWorld -> b -> IO Bool) -> b -> IO ()
connect_self_cairo_ud o (Signal sig) h ud = withCString sig \csig -> do
	ch <- G.Callback.c_self_cairo_ud h
	c_g_signal_connect (toPtr o) csig ch (toPtr ud)

connect_self_button_ud :: (G.Object.IsO a, IsPtr b) =>
	a -> Signal -> (a -> Gdk.Event.Button.B -> b -> IO Bool) -> b -> IO ()
connect_self_button_ud o (Signal sig) h ud = withCString sig \csig -> do
	ch <- G.Callback.c_self_button_ud h
	c_g_signal_connect (toPtr o) csig ch (toPtr ud)

connect_self_motion_ud :: (G.Object.IsO a, IsPtr b) =>
	a -> Signal -> (a -> Gdk.Event.Motion.M -> b -> IO Bool) -> b -> IO ()
connect_self_motion_ud o (Signal sig) h ud = withCString sig \csig -> do
	ch <- G.Callback.c_self_motion_ud h
	c_g_signal_connect (toPtr o) csig ch (toPtr ud)

foreign import capi "gtk/gtk.h g_signal_connect" c_g_signal_connect ::
	Ptr a -> CString -> G.Callback.C fun -> Ptr b -> IO ()
