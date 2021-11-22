module Main where

import Lib

main :: IO ()
main = do
	print $ c_add 321 foo
