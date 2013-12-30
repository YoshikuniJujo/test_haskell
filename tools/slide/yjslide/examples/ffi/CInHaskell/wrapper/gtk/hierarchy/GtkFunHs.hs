{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

module GtkFunHs (
	gtkInit,
	gtkMain,
	gtkWindowNew,
	gtkButtonNewWithLabel,
	gtkContainerAdd,
	gtkWidgetShow,
	gSignalConnectData,
	gtkMainQuit,
) where

import Data.Typeable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.Environment
import Foreign.Marshal
import Foreign.Storable
import Control.Applicative

import Hierarchy
import GtkFun

gtkMain :: IO ()
gtkMain = c_gtkMain

gtkWindowNew :: IO SomeGtkWindow
gtkWindowNew = SomeGtkWindow <$> c_gtkWindowNew 0

gtkButtonNewWithLabel :: String -> IO SomeGtkButton
gtkButtonNewWithLabel l =
	withCString l $ \lc -> SomeGtkButton <$> c_gtkButtonNewWithLabel lc

gtkWidgetShow :: GtkWidget -> IO ()
gtkWidgetShow = c_gtkWidgetShow . pointer

gtkContainerAdd :: GtkContainer -> GtkWidget -> IO ()
gtkContainerAdd c w = c_gtkContainerAdd (pointer c) (pointer w)

gSignalConnectData ::
	GtkWidget -> String -> (GtkWidget -> Ptr () -> IO ()) ->
		Ptr () -> Ptr () -> CInt -> IO ()
gSignalConnectData w s f p1 p2 i = withCString s $ \sc -> do
	cb <- wrapCallback (f . fromPointer)
	c_gSignalConnectData (pointer w) sc cb p1 p2 i

gtkMainQuit :: GtkWidget -> Ptr () -> IO ()
gtkMainQuit = c_gtkMainQuit . pointer

gtkInit :: [String] -> IO ()
gtkInit args = do
	allocaArray (length args) $ \ptr -> alloca $ \argc -> do
		mapM newCString args >>= pokeArray ptr
		poke argc $ fromIntegral $ length args
		c_gtkInit argc ptr
