{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import RecursionOverModules.List (List(..))
import RecursionOverModules.List qualified as L
import RecursionOverModules.NonEmpty (NonEmpty(..))
import RecursionOverModules.NonEmpty qualified as NE

main :: IO ()
main = do
	let	l = NE (1 :. NE (2 :. Nil)) :: List Int
		ne = 1 :. NE (2 :. NE (3 :. Nil)) :: NonEmpty Int
	print l
	print $ L.len l
	print ne
	print $ NE.len ne
