{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

import Try

main :: IO ()
main = do
	(fp : _) <- getArgs
	pr <- parseCFile fp
	print pr
	print $ cTranslUnitToEnums <$> pr
