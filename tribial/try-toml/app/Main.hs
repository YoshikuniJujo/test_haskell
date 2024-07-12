{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	print . parseToml =<< readFile fp
