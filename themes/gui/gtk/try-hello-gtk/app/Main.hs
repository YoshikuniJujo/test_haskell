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
	gSignalConnect win Destroy gtkMainQuit ()
	gSignalConnect win KeyPressEvent (\a b c -> print (a, b, c)) ()
	gtkMain
