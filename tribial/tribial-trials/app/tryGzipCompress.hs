{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Compress

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	compressFile compressRL ifp ofp
