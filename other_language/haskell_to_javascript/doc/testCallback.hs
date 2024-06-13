{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.JS.Prim
import GHC.JS.Foreign.Callback

main :: IO ()
main = do
	putStrLn "begin"
	main2
	main2
	putStrLn "end"

main2 :: IO ()
main2 = do
	printJS <- syncCallback1 ThrowWouldBlock printJSValAsString
	callback_example printJS
	releaseCallback printJS

printJSValAsString :: JSVal -> IO ()
printJSValAsString = putStrLn . fromJSString

foreign import javascript "((f) => { f('Example!'); })"
	callback_example :: Callback (JSVal -> IO ()) -> IO ()
