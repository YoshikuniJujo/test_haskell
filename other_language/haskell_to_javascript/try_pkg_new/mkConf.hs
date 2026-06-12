module Main (main) where

import System.Environment

import JSPackage.MkConf

main :: IO ()
main = mkConf =<< getArgs
