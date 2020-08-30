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
	gtkMain
