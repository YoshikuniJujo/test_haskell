module Main (main) where

import System.Environment

import JSPackage.Hide

main :: IO ()
main = hide =<< getArgs
