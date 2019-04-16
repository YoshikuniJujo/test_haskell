{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Lib

main :: IO ()
main = do
	print $ ifs True (123 :: Integer)
	print $ ifs False (123 :: Integer)
	print $ ifs True (Just 888 :: Maybe Integer)
	print $ ifs False (Just 888 :: Maybe Integer)
	ifs True $ print (321 :: Integer)
	ifs False $ print (321 :: Integer)
