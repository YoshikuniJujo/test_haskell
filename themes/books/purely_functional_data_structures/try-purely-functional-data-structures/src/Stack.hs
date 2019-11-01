{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stack where

import GHC.Stack (HasCallStack)

import Prelude hiding (head, tail, (++))
import qualified Prelude as P

moduleName :: String
moduleName = "try-purely-functional-data-structures.Stack"

class Stack s where
	empty :: s a
	isEmpty :: s a -> Bool
	cons :: a -> s a -> s a
	head :: HasCallStack => s a -> a
--	head :: s a -> a
	tail :: HasCallStack => s a -> s a

instance Stack [] where
	empty = []
	isEmpty = null
	cons = (:)
	head = P.head
	tail = P.tail

data CustomStack a = Nil | Cons a (CustomStack a) deriving Show

instance Stack CustomStack where
	empty = Nil
	isEmpty Nil = True
	isEmpty (Cons _ _) = False
	cons = Cons
	head (Cons x _) = x
	head Nil = error $ moduleName ++ ".Stack.CustomStack.head: Nil"
	tail (Cons _ xs) = xs
	tail Nil = error $ moduleName ++ ".Stack.CustomStack.tail: Nil"

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

update :: HasCallStack => [a] -> Int -> a -> [a]
update [] _ _ = error $ moduleName ++ ".update: index too large"
update (_ : xs) 0 y = y : xs
update (x : xs) i y | i > 0 = x : update xs (i - 1) y
update _ _ _ = error $ moduleName ++ ".update: negative index"

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xa@(_ : xs) = xa : suffixes xs
