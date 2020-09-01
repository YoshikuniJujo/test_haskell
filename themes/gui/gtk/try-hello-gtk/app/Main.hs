{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

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
	gSignalConnect win DeleteEvent (\a b c -> False <$ print (a, b, c)) ()
	gSignalConnect win Destroy gtkMainQuit ()
	gSignalConnect win KeyPressEvent printKeyPressEvent ()
	gSignalConnect win KeyReleaseEvent (\a b c -> False <$ print (a, b, c)) ()
	gSignalConnect win ButtonPressEvent (\a b c -> False <$ print (a, b, c)) ()
	gSignalConnect win ButtonReleaseEvent (\a b c -> False <$ print (a, b, c)) ()
	gtkMain

printKeyPressEvent :: Show a => Handler KeyEvent a
printKeyPressEvent w e x = False <$ do
	print w
	print =<< keyval e
	print =<< hardwareKeycode e
	print x
