{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckClassFun where

insert :: Ord a => a -> [a] -> [a]
insert = ins
	where
	ins x [] = [x]
	ins x xa@(y : xs)
		| x <= y = x : xa
		| otherwise = y : ins x xs

insert' :: Ord a => a -> [a] -> [a]
insert' = ins
	where
	(<=.) = (<=)
	ins x [] = [x]
	ins x xa@(y : xs)
		| x <=. y = x : xa
		| otherwise = y : ins x xs
