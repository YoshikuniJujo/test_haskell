module Main (main) where

import System.Environment

import JSPackage.Register

main :: IO ()
main = register =<< getArgs
