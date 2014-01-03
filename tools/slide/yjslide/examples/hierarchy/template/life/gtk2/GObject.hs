{-# LANGUAGE FlexibleInstances #-}

module GObject (
	gSignalConnect,
	module GHierarchy
) where

import Control.Applicative

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import GHierarchy

data GCallbackPtr
class GCallback c where
	gCallbackPtr :: c -> IO (FunPtr GCallbackPtr)

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))
instance GCallback (IO ()) where
	gCallbackPtr io = castFunPtr <$> wrapIO io

data GClosure

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnectData ::
	Ptr SomeGObject -> CString -> FunPtr GCallbackPtr -> Ptr () ->
	FunPtr (Ptr () -> Ptr GClosure -> IO ()) -> CInt -> IO ()

gSignalConnect :: GCallback c => SomeGObject -> String -> c -> IO ()
gSignalConnect o s c = withCString s $ \cs -> do
	cb <- gCallbackPtr c
	c_gSignalConnectData (pointer o) cs cb nullPtr nullFunPtr 0
