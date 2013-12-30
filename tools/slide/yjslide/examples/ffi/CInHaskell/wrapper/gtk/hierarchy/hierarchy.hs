{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Foreign.Ptr
import System.Environment

import Hierarchy
import GtkFunHs

main :: IO ()
main = do
	args <- getArgs
	gtkInit args
	win <- gtkWindowNew
	button <- gtkButtonNewWithLabel "Button"
	button2 <- gtkButtonNewWithLabel "Button2"
	case (castGObject win, castGObject win, castGObject button,
			castGObject button2) of
		(Just w, Just wc, Just b, Just b2) -> do
			gtkContainerAdd wc b2
			gSignalConnect b "clicked" (`gtkMainQuit` nullPtr)
			gSignalConnectData b2 "clicked" cbButton
				nullPtr destructor 0
			gSignalConnect w "destroy" (`gtkMainQuit` nullPtr)
			gtkWidgetShow w
			gtkMain
		_ -> error "bad"

cbButton :: GtkWidget -> Ptr () -> IO ()
cbButton _widget userData = print userData

destructor :: Ptr () -> Ptr () -> IO ()
destructor userData closure = do
	putStrLn "destruct"
	print userData
	print closure
