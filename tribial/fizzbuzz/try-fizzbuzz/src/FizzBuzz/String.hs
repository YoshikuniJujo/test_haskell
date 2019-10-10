{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FizzBuzz.String where

fizzbuzz :: String
fizzbuzz = mkFizzbuzz 100 ""

mkFizzbuzz :: Int -> ShowS
mkFizzbuzz 0 = id
mkFizzbuzz n = mkFizzbuzz (n - 1) . ((fz ++ "\n") ++)
	where fz = case (n `mod` 3 == 0, n `mod` 5 == 0) of
		(False, False) -> show n
		(True, False) -> "Fizz"
		(False, True) -> "Buzz"
		(True, True) -> "FizzBuzz"
