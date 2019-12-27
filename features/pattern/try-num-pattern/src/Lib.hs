{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

newtype I = I Integer deriving Show

instance Num I where
	I m + I n = I $ m + n
	I m * I n = I $ m * n
	abs (I n) = I $ abs n
	signum (I n) = I $ signum n
	fromInteger = I
	negate (I n) = I $ negate n

instance Eq I where
	I 0 == I 0 = False
	I m == I n = m == n

f :: I -> I
f 0 = 123
f 1 = 321
f n = n

{-

> f 0
I 0
> f 1
I 321
> f 2
I 2

-}
