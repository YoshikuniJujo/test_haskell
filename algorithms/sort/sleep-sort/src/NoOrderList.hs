{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module NoOrderList (NoOrderList, fromList, toList, noMaximum, noMinimum) where

import Data.List

fromList :: [a] -> NoOrderList a
fromList = NoOrderList

toList :: Ord a => NoOrderList a -> [a]
toList (NoOrderList xs) = sort xs

newtype NoOrderList a = NoOrderList [a] deriving Show

instance Eq a => Eq (NoOrderList a) where
	NoOrderList xs == NoOrderList ys = sameElems xs ys

sub :: Eq a => [a] -> a -> Maybe [a]
sub [] _ = Nothing
sub (x : xs) y
	| x == y = Just xs
	| otherwise = (x :) <$> sub xs y


sameElems :: Eq a => [a] -> [a] -> Bool
sameElems [] [] = True
sameElems (x : xs) ys = case ys `sub` x of
	Just ys' -> sameElems xs ys'
	Nothing -> False
sameElems _ _ = False

noMaximum, noMinimum :: Ord a => NoOrderList a -> a
noMaximum (NoOrderList xs) = maximum xs
noMinimum (NoOrderList xs) = minimum xs
