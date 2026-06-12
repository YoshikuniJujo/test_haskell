module Main (main) where

import System.Environment

import JSPackage.Archive

main :: IO ()
main = archive =<< getArgs
