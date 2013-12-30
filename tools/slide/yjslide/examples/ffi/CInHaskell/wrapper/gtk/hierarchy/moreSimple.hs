{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Foreign.Ptr
import System.Environment
import Data.Maybe
import Foreign.C.String
import Foreign.Marshal.Alloc

import Hierarchy
import GtkFunHs

main :: IO ()
main = do
	args <- getArgs
	gtkInit args
	win <- gtkWindowNew
	button <- gtkButtonNewWithLabel "Button"
	button2 <- gtkButtonNewWithLabel "Button2"
	hello <- newCString "hello"
	gtkContainerAdd (cast win) (cast button2)
	gSignalConnectData (cast button2) "clicked" cbButton
		(castPtr hello) destructor 0
	gSignalConnect (cast win) "destroy" (`gtkMainQuit` nullPtr)
	gtkWidgetShow (cast win)
	gtkMain

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject

cbButton :: GtkWidget -> Ptr () -> IO ()
cbButton _widget userData = print =<< peekCString (castPtr userData)

destructor :: Ptr () -> Ptr () -> IO ()
destructor userData _closure = do
	putStrLn "destruct"
--	print userData
	let cstr = castPtr userData
	print =<< peekCString cstr
	free cstr
	print =<< peekCString cstr
--	print closure
