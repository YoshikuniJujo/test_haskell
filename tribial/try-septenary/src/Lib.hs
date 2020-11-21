{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

septenary :: RealFrac x => x -> [Int]
septenary 0 = []
septenary x = n : septenary (7 * x')
	where
	(n, x') = properFraction x

unSeptenary :: RealFrac x => [Int] -> x
unSeptenary [] = 0
unSeptenary (n : ns) = fromIntegral n + unSeptenary ns / 7

foo 0 = []
foo x = n : foo (10 * x')
	where
	(n, x') = properFraction x
