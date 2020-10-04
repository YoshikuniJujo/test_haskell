{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Gobject.SignalConnect where

import Foreign.Ptr
import Foreign.C
import Foreign.Tools

import System.Gobject.Hierarchy

class Signal s where
	type Reciever s
	type Callback s o a
	type CCallback s o a
	signalName :: s -> String
	callbackToCCallback :: (GObject o, AsPointer a) => o -> Callback s o a -> CCallback s o a
	wrapCCallback :: CCallback s o a -> IO (FunPtr (CCallback s o a))

gSignalConnect :: forall o s a .
	(GObject o, Signal s, GObject (Reciever s), AsPointer a) =>
	o -> s -> (Callback s o a) -> a -> IO ()
gSignalConnect o s c x = do
	_ <- gCastObjectIo o :: IO (Reciever s)
	withCString (signalName s) \cs -> asPointer x \px -> pointer o \po -> do
		cb <- wrapCCallback @s @o @a $ callbackToCCallback @s @o @a o c
		c_g_signal_connect po cs cb px

foreign import capi "glib-object.h g_signal_connect" c_g_signal_connect ::
	Ptr o -> CString -> FunPtr f -> Ptr a -> IO ()

-- conv :: Pointer a => (a -> b) -> IO (Ptr a -> b)
-- conv f = 

-- Ptr a -> IO a
