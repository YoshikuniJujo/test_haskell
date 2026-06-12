module Main (main) where

import System.Environment

import JSPackage.Expose

main :: IO ()
main = expose =<< getArgs
