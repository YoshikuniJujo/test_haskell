#include <gtk/gtk.h>

{-# LANGUAGE FlexibleInstances #-}

module GObject (
	GCallback(..),
	Pointable(..),
	GClosure,
	gSignalConnect,
	gSignalConnectData,
	zero,
	module GHierarchy
) where

import Control.Applicative

import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.Types
import Foreign.C.String

import GHierarchy

data GCallbackPtr
class GCallback c where
	gCallbackPtr :: c -> IO (FunPtr GCallbackPtr)

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))
instance GCallback (IO ()) where
	gCallbackPtr io = castFunPtr <$> wrapIO io

class Pointable p where
	toNullPointer :: p -> IO (Ptr ())
	fromNullPointer :: Ptr () -> IO p
	freePointer :: p -> Ptr () -> IO ()

class ListPointable p where
	toNullPointerL :: [p] -> IO (Ptr ())
	fromNullPointerL :: Ptr () -> IO [p]
	freePointerL :: [p] -> Ptr () -> IO ()

instance ListPointable p => Pointable [p] where
	toNullPointer = toNullPointerL
	fromNullPointer = fromNullPointerL
	freePointer = freePointerL

instance ListPointable Char where
	toNullPointerL str = castPtr <$> newCString str
	fromNullPointerL = peekCString . castPtr
	freePointerL _ = free

foreign import ccall "wrapper" wrapGClosureNotify ::
	(Ptr () -> Ptr GClosure -> IO ()) ->
	IO (FunPtr (Ptr () -> Ptr GClosure -> IO ()))

data GClosure = GClosure (Ptr GClosure) deriving Show

data GConnectFlags = GConnectFlags CInt deriving Show
#enum GConnectFlags, GConnectFlags, G_CONNECT_AFTER, G_CONNECT_SWAPPED
zero = GConnectFlags 0

foreign import ccall "gtk/gtk.h g_signal_connect_data" c_gSignalConnectData ::
	Ptr SomeGObject -> CString -> FunPtr GCallbackPtr -> Ptr () ->
	FunPtr (Ptr () -> Ptr GClosure -> IO ()) -> CInt -> IO ()

gSignalConnect :: GCallback c => SomeGObject -> String -> c -> IO ()
gSignalConnect o s c = withCString s $ \cs -> do
	cb <- gCallbackPtr c
	c_gSignalConnectData (pointer o) cs cb nullPtr nullFunPtr 0

gSignalConnectData :: (GCallback c, Pointable p) => SomeGObject -> String ->
	c -> p -> GConnectFlags -> IO ()
gSignalConnectData inst sg hndl dat flags =
	gSignalConnectDataGen inst sg hndl dat destroy flags
	where
	destroy p _ = freePointer dat p

gSignalConnectDataGen :: (GCallback c, Pointable p) => SomeGObject -> String ->
	c -> p -> (Ptr () -> GClosure -> IO ()) -> GConnectFlags -> IO ()
gSignalConnectDataGen inst sg hndl dat destroy (GConnectFlags flags) =
	withCString sg $ \cs -> do
		cb <- gCallbackPtr hndl
		dst <- wrapGClosureNotify $ getDestroy destroy
		pd <- toNullPointer dat
		c_gSignalConnectData (pointer inst) cs cb pd
			dst flags
	where
	getDestroy f d c = do
--		pd <- fromNullPointer d
		f d (GClosure c)
