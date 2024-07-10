{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import Lib

main :: IO ()
main = do
	src : _ <- getArgs
	print $ go src
