module Main (main) where

import System.Environment

import JSPackage.Unregister

main :: IO ()
main = unregister =<< getArgs
