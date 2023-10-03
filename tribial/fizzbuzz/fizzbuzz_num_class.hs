{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

main :: IO ()
main = print @FizzBuzz `mapM_` [0 .. 100]

newtype FizzBuzz = FizzBuzz Int deriving (Eq, Ord, Enum, Num, Real, Integral)

instance Show FizzBuzz where
	show n = case (n `mod` 3, n `mod` 5) of
		(0, 0) -> "FizzBuzz"
		(0, _) -> "Fizz"
		(_, 0) -> "Buzz"
		_ -> show $ toInteger n
