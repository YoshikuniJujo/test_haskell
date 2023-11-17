{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.IO.Handle.FD
import System.IO hiding (openFile)

main :: IO ()
main = do
	h <- openFile "../try_fifo" ReadWriteMode
	putStrLn "file opened"
	hGetContents h >>= putStrLn
