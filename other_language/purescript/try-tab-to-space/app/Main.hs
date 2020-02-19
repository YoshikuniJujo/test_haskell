{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import Lib

main :: IO ()
main = do
	dr : _ <- getArgs
	purstAndPurs dr >>= mapM_ (\(s, d) -> writeFile d . tabToSpace 0 =<< readFile s)

tabToSpace :: Int -> String -> String
tabToSpace _ "" = ""
tabToSpace _ ('\n' : cs) = '\n' : tabToSpace 0 cs
tabToSpace n ('\t' : cs) = replicate (8 - n `mod` 8) ' ' ++ tabToSpace 0 cs
tabToSpace n (c : cs) = c : tabToSpace (n + 1) cs
