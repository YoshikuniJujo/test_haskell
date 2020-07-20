{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckClassFun where

insert :: Ord a => a -> [a] -> [a]
insert = ins
	where
	ins x [] = [x]
	ins x xa@(y : xs)
		| x `compare` y /= GT = x : xa
--		| x <= y = x : xa
		| otherwise = y : ins x xs

insert' :: forall a . Ord a => a -> [a] -> [a]
insert' = insertBy compare

insertBy cmp = ins
	where
--	(<=.) :: Ord a => a -> a -> Bool
--	(<=.) :: a -> a -> Bool
--	(<=.) = (<=)
--	cmp :: a -> a -> Ordering
--	cmp = compare
	ins x [] = [x]
	ins x xa@(y : xs)
		| x `cmp` y /= GT = x : xa
--		| x <=. y = x : xa
		| otherwise = y : ins x xs
