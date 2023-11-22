{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Gobject (GObject(..), Event(..), gSignalConnect) where

import Foreign.Ptr
import Foreign.C
import Foreign.Tools

newtype GObject = GObject (Ptr GObject) deriving Show

class Event e where
	type Handler e a
	type CHandler e a
	eventName :: e -> String
	handlerToCHandler :: AsPointer a => Handler e a -> CHandler e a
	g_callback :: CHandler e a -> IO (FunPtr (CHandler e a))

gSignalConnect :: forall e a . (Event e, AsPointer a) => GObject -> e -> Handler e a -> a -> IO ()
gSignalConnect (GObject pw) e (handlerToCHandler @e @a -> h) x = do
	cs <- newCString $ eventName e
	cb <- castFunPtr <$> g_callback @e @a h
	asPointer x $ c_g_signal_connect pw cs cb

foreign import capi "glib-object.h g_signal_connect" c_g_signal_connect ::
	Ptr GObject -> CString -> FunPtr () -> Ptr a -> IO ()
