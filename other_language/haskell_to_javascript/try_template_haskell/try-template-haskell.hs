{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Foo

main :: IO ()
main = do
	putStrLn "Hello, Template Haskell!"
	putStrLn $ "foo(5) - 2 = " ++ show ($(foo(5)) - 2 :: Integer)
