{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Signal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.String
import Data.String
import Data.Int

import Stopgap.Data.Ptr

#include <gtk/gtk.h>

data Signal = Signal String deriving Show

instance IsString Signal where
	fromString = Signal

connect :: forall a b . (IsPtr a, IsPtr b) => a -> Signal -> (a -> b -> IO ()) -> b -> IO ()
connect x (Signal sig) h ud = withCString sig \csig -> wrapHandler h \ch ->
	c_g_signal_connect @(Tag a) @(Tag b) (toPtr @a x) csig ch (toPtr ud)

foreign import capi "gtk/gtk.h g_signal_connect" c_g_signal_connect ::
	Ptr a -> CString -> FunPtr (Ptr a -> Ptr b -> IO ()) -> Ptr b -> IO ()

wrapHandler :: (IsPtr a, IsPtr b) => (a -> b -> IO ()) ->
	(FunPtr (Ptr (Tag a) -> Ptr (Tag b) -> IO ()) -> IO c) -> IO c
wrapHandler h f = do
	let	g px pud = h (fromPtr px) (fromPtr pud)
	f =<< c_wrap_handler g

foreign import ccall "wrapper" c_wrap_handler ::
	(Ptr a -> Ptr b -> IO ()) -> IO (FunPtr (Ptr a -> Ptr b -> IO ()))

connectOpen :: forall a gf b . (IsPtr a, IsPtr gf, IsPtr b) =>
	a -> Signal -> HandlerOpen a gf b -> b -> IO ()
connectOpen x (Signal sig) h ud = withCString sig \csig -> do
	ch <- wrapHandlerOpen h
	c_g_signal_connect_open @(Tag a) @(Tag gf) @(Tag b) (toPtr x) csig ch (toPtr ud)

type HandlerOpen a gf b = a -> [gf] -> String -> b -> IO ()

type CHandlerOpen a gf b =
	Ptr a -> Ptr (Ptr gf) -> #{type gint} -> CString -> Ptr b -> IO ()

handlerOpenToC ::
	(IsPtr a, IsPtr gf, IsPtr b) =>
	HandlerOpen a gf b -> CHandlerOpen (Tag a) (Tag gf) (Tag b)
handlerOpenToC h pa ppgf n cstr pb = do
	pgfs <- peekArray (fromIntegral n) ppgf
	str <- peekCString cstr
	h (fromPtr pa) (fromPtr <$> pgfs) str (fromPtr pb)

foreign import capi "gtk/gtk.h g_signal_connect" c_g_signal_connect_open ::
	Ptr a -> CString -> FunPtr (CHandlerOpen a gf b) -> Ptr b -> IO ()

wrapHandlerOpen :: (IsPtr a, IsPtr gf, IsPtr b) => HandlerOpen a gf b ->
	IO (FunPtr (CHandlerOpen (Tag a) (Tag gf) (Tag b)))
wrapHandlerOpen h = c_wrap_handler_open $ handlerOpenToC h

foreign import ccall "wrapper" c_wrap_handler_open ::
	CHandlerOpen a gf b -> IO (FunPtr (CHandlerOpen a gf b))

foreign import capi "gtk/gtk.h G_CALLBACK" c_G_CALLBACK ::
	FunPtr (Ptr a -> Ptr b -> IO ()) -> FunPtr (Ptr a -> Ptr b -> IO ())
