{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Paths_try_hpack_data_dir

main :: IO ()
main = do
	putStrLn "Slozsoft"
	putStrLn =<< getDataFileName "bar.txt"
