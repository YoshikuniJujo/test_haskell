{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LazyQueue where

import GHC.Stack (HasCallStack)
import Control.Exception

data Queue a = Queue Int [a] Int [a]

check :: Queue a -> Queue a
check q@(Queue lenf f lenr r)
	| lenr <= lenf = q
	| otherwise = Queue (lenf + lenr) (f ++ reverse r) 0 []

snoc :: Queue a -> a -> Queue a
snoc (Queue lenf f lenr r) x = check $ Queue lenf f (lenr + 1) (x : r)

data Empty = Empty deriving Show
instance Exception Empty

head :: HasCallStack => Queue a -> a
head (Queue _ [] _ _) = throw Empty
head (Queue _ (x : _) _ _) = x

tail :: HasCallStack => Queue a -> Queue a
tail (Queue _ [] _ _) = throw Empty
tail (Queue lenf (_ : f') lenr r) = check $ Queue (lenf - 1) f' lenr r
