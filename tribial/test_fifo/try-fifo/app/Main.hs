{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.IO

main :: IO ()
main = do
	h <- openFile "../try_fifo" ReadMode
	putStrLn "file opened"
	hGetContents h >>= putStrLn
