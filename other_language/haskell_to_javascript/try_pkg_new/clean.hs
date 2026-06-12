module Main (main) where

import System.Environment

import JSPackage.Clean

main :: IO ()
main = clean =<< getArgs
