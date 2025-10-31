module Main where

import System.Environment
import System.File.Png.Lifegame

main :: IO ()
main = do
	fp : _ <- getArgs
	print =<< readBoard fp
