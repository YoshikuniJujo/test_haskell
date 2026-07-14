{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value

import Tools

main :: IO ()
main = do
	generateKeyFromPassword "foo" "salt"
	pure ()

generateKeyFromPassword :: String -> String -> IO JSVal
generateKeyFromPassword password salt = do
	encoder <- newTextEncoder
	JS.Value.consoleLog encoder
	JS.Value.consoleLog =<< encode encoder password
	pure undefined
