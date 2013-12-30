{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

module GtkFunHs (
	gtkInit,
	gtkMain,
	gtkWindowNew,
	gtkButtonNewWithLabel,
	gtkContainerAdd,
	gtkWidgetShow,
	gSignalConnect,
	gSignalConnectData,
	gtkMainQuit,
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
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
		Ptr () -> (Ptr () -> Ptr () -> IO ()) -> CInt -> IO ()
gSignalConnectData w s f p1 de i = withCString s $ \sc -> do
	cb <- wrapCallback (f . fromPointer)
	p2 <- wrapDestructor de
	c_gSignalConnectData (pointer w) sc cb p1 p2 i

gSignalConnect :: GtkWidget -> String -> (GtkWidget -> IO ()) -> IO ()
gSignalConnect w s f = gSignalConnectData w s (const . f) nullPtr nullDest 0

nullDest :: Ptr () -> Ptr () -> IO ()
nullDest p c = do
	putStrLn "null destructor"
	print p
	print c
	return ()

gtkMainQuit :: GtkWidget -> Ptr () -> IO ()
gtkMainQuit = c_gtkMainQuit . pointer

gtkInit :: [String] -> IO ()
gtkInit args = do
	allocaArray (length args) $ \ptr -> alloca $ \argc -> do
		mapM newCString args >>= pokeArray ptr
		poke argc $ fromIntegral $ length args
		c_gtkInit argc ptr
