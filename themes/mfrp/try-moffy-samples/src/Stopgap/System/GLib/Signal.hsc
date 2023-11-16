{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Signal where

import Foreign.Ptr
import Foreign.C.String
import Data.String

import Stopgap.Data.Ptr

connect :: forall a b . (IsPtr a, IsPtr b) => a -> Signal -> (a -> b -> IO ()) -> b -> IO ()
connect x (Signal sig) h ud = withCString sig \csig -> wrapHandler h \ch ->
	c_g_signal_connect @(Tag a) @(Tag b) (toPtr @a x) csig ch (toPtr ud)

data Signal = Signal String deriving Show

instance IsString Signal where
	fromString = Signal

foreign import capi "gtk/gtk.h g_signal_connect" c_g_signal_connect ::
	Ptr a -> CString -> FunPtr (Ptr a -> Ptr b -> IO ()) -> Ptr b -> IO ()

wrapHandler :: (IsPtr a, IsPtr b) => (a -> b -> IO ()) ->
	(FunPtr (Ptr (Tag a) -> Ptr (Tag b) -> IO ()) -> IO c) -> IO c
wrapHandler h f = do
	let	g px pud = h (fromPtr px) (fromPtr pud)
	f =<< c_wrap_handler g

foreign import ccall "wrapper" c_wrap_handler ::
	(Ptr a -> Ptr b -> IO ()) -> IO (FunPtr (Ptr a -> Ptr b -> IO ()))

foreign import capi "gtk/gtk.h G_CALLBACK" c_G_CALLBACK ::
	FunPtr (Ptr a -> Ptr b -> IO ()) -> FunPtr (Ptr a -> Ptr b -> IO ())
