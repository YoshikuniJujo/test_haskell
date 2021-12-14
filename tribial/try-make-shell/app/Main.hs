{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.IO
import System.Process

main :: IO ()
main = putStr "Shell> " >> hFlush stdout >> getLine >>= callCommand >> main
