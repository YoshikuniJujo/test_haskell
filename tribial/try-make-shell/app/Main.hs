{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Process

main :: IO ()
main = callCommand =<< getLine
