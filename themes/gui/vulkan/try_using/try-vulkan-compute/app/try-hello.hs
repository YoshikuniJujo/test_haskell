{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

foreign import ccall "hello" c_hello :: IO ()

main :: IO ()
main = c_hello
