{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.JS.Value.Navigator qualified as JS.Navigator
import GHC.JS.Value.Window qualified as JS.Window
import GHC.JS.Value.Document qualified as JS.Document
import GHC.JS.Value.Date qualified as JS.Date

main :: IO ()
main = do
	putStrLn "Hello"
	print $ JS.Window.document JS.Window.w
	print $ JS.Navigator.n
