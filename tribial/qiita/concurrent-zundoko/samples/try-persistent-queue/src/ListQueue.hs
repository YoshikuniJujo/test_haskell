{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ListQueue (
	ListQueue, empty, snoc, uncons, cons, head, tail, snocAll, consAll
	) where

import Prelude hiding (head, tail)

import Queue (Queue(..), ConsQueue(..), head, tail, snocAll, consAll)

newtype ListQueue a = ListQueue [a] deriving Show

snocList :: [a] -> a -> [a]
snocList [] y = [y]
snocList (x : xs) y = x : snocList xs y

instance Queue ListQueue where
	empty = ListQueue []
	snoc (ListQueue xs) x = ListQueue $ snocList xs x
	uncons (ListQueue []) = Nothing
	uncons (ListQueue (x : xs)) = Just (x, ListQueue xs)

instance ConsQueue ListQueue where
	cons x (ListQueue xs) = ListQueue $ x : xs
