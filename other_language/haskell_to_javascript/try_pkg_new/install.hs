module Main (main) where

import System.Environment

import JSPackage.Install

main :: IO ()
main = install =<< getArgs
