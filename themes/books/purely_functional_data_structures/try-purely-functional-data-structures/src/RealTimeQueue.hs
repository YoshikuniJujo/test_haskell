{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RealTimeQueue where

import GHC.Stack (HasCallStack)
import Control.Exception

data Queue a = Queue [a] [a] [a] deriving Show

empty :: Queue a
empty = Queue [] [] []

snoc :: Queue a -> a -> Queue a
snoc (Queue f r s) x = exec $  Queue f (x : r) s

uncons :: Queue a -> Maybe (a, Queue a)
uncons (Queue [] _ _) = Nothing
uncons (Queue (x : f) r s) = Just (x, exec $ Queue f r s)

exec :: Queue a -> Queue a
exec (Queue f r (_ : s)) = Queue f r s
exec (Queue f r []) = let f' = rotate f r [] in Queue f' [] f'

rotate :: HasCallStack => [a] -> [a] -> [a] -> [a]
rotate [] [y] a = y : a
rotate (x : xs) (y : ys) a = x : rotate xs ys (y : a)
rotate _ _ _ = error "bad format"

data Empty = Empty deriving Show
instance Exception Empty

head :: HasCallStack => Queue a -> a
head (Queue [] _ _) = throw Empty
head (Queue (x : _) _ _) = x

tail :: HasCallStack => Queue a -> Queue a
tail (Queue [] _ _) = throw Empty
tail (Queue (_ : f) r s) = exec $ Queue f r s
