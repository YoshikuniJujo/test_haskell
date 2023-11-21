{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Timeout where

import Foreign.Ptr
import Data.Word
import Data.Int
import Stopgap.Data.Ptr

#include <gtk/gtk.h>

add :: IsPtr ud => #{type guint} -> (ud -> IO Bool) -> ud -> IO #{type guint}
add i f ud = do
	cf <- wrapHandler f
	c_g_timeout_add i cf $ toPtr ud

foreign import ccall "g_timeout_add" c_g_timeout_add ::
	#{type guint} -> FunPtr (Ptr ud -> IO #{type gboolean}) -> Ptr ud ->
	IO #{type guint}

wrapHandler :: IsPtr ud =>
	(ud -> IO Bool) -> IO (FunPtr (Ptr (Tag ud) -> IO #{type gboolean}))
wrapHandler h = do
	let	g pud = boolToGboolean <$> h (fromPtr pud)
	c_wrap_handler g

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean = \case False -> #{const FALSE}; True -> #{const TRUE}

foreign import ccall "wrapper" c_wrap_handler ::
	(Ptr ud -> IO #{type gboolean}) ->
	IO (FunPtr (Ptr ud -> IO #{type gboolean}))
