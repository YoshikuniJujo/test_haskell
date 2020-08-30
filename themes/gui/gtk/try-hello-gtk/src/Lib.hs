module Lib where

foreign import ccall "hello_main" c_hello_main :: IO ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
