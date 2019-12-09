{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Queue (Queue(..), ConsQueue(..), head, tail, snocAll, consAll) where

import Data.List (foldl')

import Prelude hiding (head, tail)

class Queue q where
	empty :: q a
	snoc :: q a -> a -> q a
	uncons :: q a -> Maybe (a, q a)

class Queue q => ConsQueue q where
	cons :: a -> q a -> q a

head :: Queue q => q a -> Maybe a
head = (fst <$>) . uncons

tail :: Queue q => q a -> Maybe (q a)
tail = (snd <$>) . uncons

snocAll :: Queue q => q a -> [a] -> q a
snocAll = foldl' snoc

consAll :: ConsQueue q => [a] -> q a -> q a
consAll = flip $ foldr cons
