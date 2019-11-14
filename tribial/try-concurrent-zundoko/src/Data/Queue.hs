{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Queue (Queue(..), ConsQueue(..), isEmpty, head, tail) where

import Prelude hiding (head, tail)
import Data.Maybe (isNothing)

class Queue q where
	empty :: q a
	snoc :: q a -> a -> q a
	uncons :: q a -> Maybe (a, q a)

class Queue cq => ConsQueue cq where
	cons :: a -> cq a -> cq a

isEmpty :: Queue q => q a -> Bool
isEmpty = isNothing . uncons

head :: Queue q => q a -> Maybe a
head = (fst <$>) . uncons

tail :: Queue q => q a -> Maybe (q a)
tail = (snd <$>) . uncons
