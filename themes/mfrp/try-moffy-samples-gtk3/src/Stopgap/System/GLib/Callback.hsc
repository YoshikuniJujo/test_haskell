{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Callback where

import Foreign.Ptr
import Foreign.Concurrent
import Control.Monad.ST
import Data.Int
import Data.CairoContext
import Stopgap.Data.Ptr

#include <gtk/gtk.h>

data CTag

newtype C fun = C (FunPtr CTag) deriving Show

foreign import capi "gtk/gtk.h G_CALLBACK" c_G_CALLBACK :: FunPtr fun -> C fun

c_ab :: (IsPtr a, IsPtr b) =>
	(a -> b -> IO ()) -> IO (C (Ptr (Tag a) -> Ptr (Tag b) -> IO ()))
c_ab f = do
	let	f' x y = f (fromPtr x) (fromPtr y)
	c_G_CALLBACK <$> c_wrap_callback_ab f'

foreign import ccall "wrapper" c_wrap_callback_ab ::
	(Ptr a -> Ptr b -> IO ()) -> IO (FunPtr (Ptr a -> Ptr b -> IO ()))

c_ab_bool :: (IsPtr a, IsPtr b) =>
	(a -> b -> IO Bool) -> IO (C (Ptr (Tag a) -> Ptr (Tag b) -> IO #{type gboolean}))
c_ab_bool f = do
	let	f' x y = boolToGboolean <$> f (fromPtr x) (fromPtr y)
	c_G_CALLBACK <$> c_wrap_callback_ab_bool f'

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean = \case False -> #{const FALSE}; True -> #{const TRUE}

foreign import ccall "wrapper" c_wrap_callback_ab_bool ::
	(Ptr a -> Ptr b -> IO #{type gboolean}) ->
	IO (FunPtr (Ptr a -> Ptr b -> IO #{type gboolean}))

c_void_void :: IO () -> IO (C (IO ()))
c_void_void f = c_G_CALLBACK <$> c_wrap_callback_void_void f

foreign import ccall "wrapper" c_wrap_callback_void_void ::
	IO () -> IO (FunPtr (IO ()))

c_self_cairo_ud :: (IsPtr a, IsPtr b) =>
	(a -> CairoT r RealWorld -> b -> IO Bool) ->
	IO (C (Ptr (Tag a) -> Ptr (CairoT r RealWorld) -> Ptr (Tag b) -> IO #{type gboolean}))
c_self_cairo_ud f = do
	let	f' x cr y = boolToGboolean <$> do
			cr' <- CairoT <$> newForeignPtr cr (pure ())
			f (fromPtr x) cr' (fromPtr y)
	c_G_CALLBACK <$> c_wrap_callback_self_cairo_ud f'

foreign import ccall "wrapper" c_wrap_callback_self_cairo_ud ::
	(Ptr a -> Ptr (CairoT r s) -> Ptr b -> IO #{type gboolean}) ->
	IO (FunPtr (Ptr a -> Ptr (CairoT r s) -> Ptr b -> IO #{type gboolean}))
