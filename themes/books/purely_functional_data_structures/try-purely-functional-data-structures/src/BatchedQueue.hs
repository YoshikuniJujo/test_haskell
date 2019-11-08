{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BatchedQueue where

import Prelude hiding (head, tail)

import GHC.Stack (HasCallStack)	
import Control.Exception

class Queue q where
	empty :: q a
	isEmpty :: q a -> Bool
	snoc :: q a -> a -> q a
	head :: HasCallStack => q a -> a
	tail :: HasCallStack => q a -> q a

data Empty = Empty deriving Show

instance Exception Empty

bad :: a
bad = throw Empty

data Q a = Q [a] [a] deriving Show

instance Queue Q where
	empty = Q [] []
	isEmpty (Q [] []) = True
	isEmpty _ = False
--	snoc (Q [] _) x = Q [x] []
	snoc (Q f r) x = checkf $ Q f (x : r)
	head (Q (x : _) _) = x
	head (Q [] _) = bad
--	tail (Q [_] r) = Q (reverse r) []
	tail (Q (_ : f) r) = checkf $ Q f r
	tail (Q [] _) = bad

checkf :: Q a -> Q a
checkf (Q [] r) = Q (reverse r) []
checkf q = q
