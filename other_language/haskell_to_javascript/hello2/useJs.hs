{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

main :: IO ()
main = do
	putStrLn "foobar"
	js_foo

foreign import javascript "(() => { foo(); })" js_foo :: IO ()
