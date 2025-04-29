module Main where

import System.Environment
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Compress

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	compressFile compressRL ifp ofp
