module Main where

import GHC.JS.Value
import GHC.JS.Value.Object
import GHC.JS.Value.String

main :: IO ()
main = putStrLn . fromJS $ toJS "foobar"
