{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Language.Haskell.TH

import Paths_try_compile_time_file

runIO do
	putStrLn "*** COMPILE TIME ***"
	fp <- getDataFileName "foo.txt"
	putStrLn fp
	putStr =<< readFile fp
	pure []

main :: IO ()
main = do
	fp <- getDataFileName "foo.txt"
	putStrLn fp
	putStr =<< readFile fp
