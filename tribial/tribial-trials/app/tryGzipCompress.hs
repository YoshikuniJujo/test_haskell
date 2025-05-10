{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.Compress
import Yaftee.UseFTCQ.Pipe.Gzip.Compress.RunLength

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	compressFile' compressRL ifp ofp
--	debugFile compressRL ifp ofp
