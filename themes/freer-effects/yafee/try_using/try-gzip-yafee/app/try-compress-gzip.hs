module Main where

import System.Environment
import Compress

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	compressFile ifp ofp
