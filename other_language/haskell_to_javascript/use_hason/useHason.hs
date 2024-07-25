module Main (main) where

import System.Environment

import Hason
import Hason.Eval

main :: IO ()
main = print . eval =<< readFile . head =<< getArgs
