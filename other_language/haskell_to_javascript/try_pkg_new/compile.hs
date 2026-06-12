module Main (main) where

import System.Environment

import JSPackage.Compile

main :: IO ()
main = compile =<< getArgs
