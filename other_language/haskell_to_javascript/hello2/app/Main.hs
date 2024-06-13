{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foo
import Foo.Bar

foreign import javascript "((x, y) => { return x + y; })"
	js_add :: Int -> Int -> Int

foreign import javascript "((x, y) => { return x * y; })"
	js_mul :: Int -> Int -> Int

main :: IO ()
main = do
	putStrLn "Hello, JavaScript!"
	print $ js_add 3 8
	print $ js_mul 3 8
	print foo
	print bar
