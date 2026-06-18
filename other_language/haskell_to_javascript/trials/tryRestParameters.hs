{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.JS.Prim (JSVal, toJSInt)

main :: IO ()
main = do
	putStrLn "foobar"
	js_test1 js_sample

foreign import javascript "(() => { const a = new Array(10, 11, 12, 13, 14, 15); return a; })"
	js_sample :: JSVal

foreign import javascript "((x) => { console.log(...x) })"
	js_test1 :: JSVal -> IO ()
