{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

main :: IO ()
main = do
	writeFile "hello.txt" "Hello"
