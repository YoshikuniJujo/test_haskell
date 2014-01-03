{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GObject (
	GObject(..),
	gSignalConnect,
) where

import Control.Applicative

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

data GCallbackPtr
class GCallback c where
	gCallbackPtr :: c -> IO (FunPtr GCallbackPtr)

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))

instance GCallback (IO ()) where
	gCallbackPtr io = castFunPtr <$> wrapIO io

data GObjectPtr
class GObject o where
	gObjectPtr :: o -> Ptr GObjectPtr

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnect ::
	Ptr GObjectPtr -> CString -> FunPtr GCallbackPtr -> Ptr () ->
	Ptr () -> Ptr () -> IO ()

gSignalConnect :: (GObject o, GCallback c) => o -> String -> c -> IO ()
gSignalConnect o s c = withCString s $ \cs -> do
	cb <- gCallbackPtr c
	c_gSignalConnect (gObjectPtr o) cs cb nullPtr nullPtr nullPtr
