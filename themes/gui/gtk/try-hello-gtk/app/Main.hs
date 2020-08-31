{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import System.Environment

import Lib
import Values

main :: IO ()
main = do
	as <- getArgs
	as' <- gtkInit as
	print as'
	win <- gtkWindowNew gtkWindowToplevel
	gtkWidgetShowAll win
	gSignalConnect win Destroy gtkMainQuit ()
	gSignalConnect win KeyPressEvent (\a b c -> boolToGBoolean False <$ printKeyPressEvent a b c) ()
	gSignalConnect win KeyReleaseEvent (\a b c -> boolToGBoolean False <$ print (a, b, c)) ()
	gSignalConnect win ButtonPressEvent (\a b c -> boolToGBoolean False <$ print (a, b, c)) ()
	gSignalConnect win ButtonReleaseEvent (\a b c -> boolToGBoolean False <$ print (a, b, c)) ()
	gtkMain

printKeyPressEvent :: GtkWidget -> GdkEventKey -> Ptr a -> IO ()
printKeyPressEvent w e x = do
	print w
	print =<< keyval e
	print =<< hardwareKeycode e
	print x
