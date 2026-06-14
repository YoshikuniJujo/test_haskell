module Main (main) where

import System.Environment

import JSPackage.Place

main :: IO ()
main = place =<< getArgs
