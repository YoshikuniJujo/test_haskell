{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.JS.Prim
import GHC.JS.Foreign.Callback

main :: IO ()
main = do
	print (123 :: Int)
	print $ fromJSInt js123
	print . map fromJSInt =<< fromJSArray jsArray123
	print . fromJSInt =<< getProp jsObjectFooBar "foo"
	main2
	main3
	main2
	main3
--	main3
--	main2
--	main3
--	main2
--	main3
--	main2
--	main3

foreign import javascript "(() => { return 123; })"
	js123 :: JSVal

foreign import javascript "(() => { return [1, 2, 3]; })"
	jsArray123 :: JSVal

foreign import javascript "(() => { return { foo: 123, bar : 321 }; })"
	jsObjectFooBar :: JSVal

foreign import javascript "((f) => { f('Example!'); })"
	callback_example :: Callback (JSVal -> IO ()) -> IO ()

printJSValAsString :: JSVal -> IO ()
printJSValAsString = putStrLn . fromJSString

main2 :: IO ()
main2 = do
	printJS <- syncCallback1 ThrowWouldBlock printJSValAsString
	callback_example printJS
	releaseCallback printJS

foreign import javascript "((f) => { f(); })"
	callback0_example :: Callback (IO ()) -> IO ()

main3 :: IO ()
main3 = do
	printFoo <- syncCallback ThrowWouldBlock $ putStrLn "Foo"
	callback0_example printFoo
	releaseCallback printFoo
