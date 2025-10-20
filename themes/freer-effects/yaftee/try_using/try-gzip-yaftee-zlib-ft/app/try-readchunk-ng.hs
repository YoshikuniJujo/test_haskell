{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Png
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	putStrLn fp
