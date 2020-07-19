{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import CheckClassFun

main :: IO ()
main = do
	print . last $ {-# SCC "check_insert" #-} insert (10 ^ (5 :: Int) + 1) [1 .. 10 ^ (5 :: Int) :: Int]
	print . last $ {-# SCC "check_insert'" #-} insert' (10 ^ (5 :: Int) + 1) [1 .. 10 ^ (5 :: Int) :: Int]
