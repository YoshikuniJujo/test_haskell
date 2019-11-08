{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Queue (Queue(..), Deque(..)) where

import GHC.Stack (HasCallStack)
import Control.Exception
import Data.Maybe

data Empty = Empty deriving Show
instance Exception Empty

class Queue q where
	empty :: q a
	snoc :: q a -> a -> q a
	uncons :: q a -> Maybe (a, q a)

	isEmpty :: q a -> Bool
	isEmpty q = isNothing $ uncons q
	head :: HasCallStack => q a -> a
	head q = case uncons q of
		Nothing -> throw Empty
		Just (x, _) -> x
	tail :: HasCallStack => q a -> q a
	tail q = case uncons q of
		Nothing -> throw Empty
		Just (_, q') -> q'

class Queue dq => Deque dq where
	rev :: dq a -> dq a

	cons :: a -> dq a -> dq a
	cons x dq = rev $ snoc (rev dq) x

	unsnoc :: dq a -> Maybe (dq a, a)
	unsnoc dq = case uncons dq of
		Nothing -> Nothing
		Just (x, dq') -> Just (rev dq', x)

	last :: HasCallStack => dq a -> a
	last dq = case unsnoc dq of
		Nothing -> throw Empty
		Just (_, x) -> x
	init :: HasCallStack => dq a -> dq a
	init dq = case unsnoc dq of
		Nothing -> throw Empty
		Just (dq', _) -> dq'
