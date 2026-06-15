module Main (main) where

import System.Environment

import JSPackage.Unplace

main :: IO ()
main = unplace =<< getArgs
