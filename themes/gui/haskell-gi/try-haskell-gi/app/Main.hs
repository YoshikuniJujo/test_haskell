{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified GI.Gtk as G
import Data.GI.Base

main :: IO ()
main = do
	G.init Nothing
	win <- new G.Window [ #title :=  "Hi there " ]
	on win #destroy G.mainQuit
	button <- new G.Button [ #label := "Click me" ]
	on button #clicked (set button [ #sensitive := False, #label := "Thanks for clicking me" ])
	#add win button
	#showAll win

	G.main
