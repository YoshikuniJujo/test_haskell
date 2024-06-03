{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

main :: IO ()
main = do
	fn : _ <- getArgs
	cnt <- readFile fn
	writeFile (fn ++ "_bak") $ tabToSpaces cnt

tabToSpaces :: String -> String
tabToSpaces "" = ""
tabToSpaces ('\t' : cs) = replicate 8 ' ' ++ tabToSpaces cs
tabToSpaces (c : cs) = c : tabToSpaces cs
