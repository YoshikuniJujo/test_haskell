{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PhysicistsQueue where

import GHC.Stack (HasCallStack)
import Control.Exception

data List a = Nil | a :! !(List a) deriving Show

(++!) :: List a -> List a -> List a
Nil ++! ys = ys
(x :! xs) ++! ys = x :! (xs ++! ys)

rev :: List a -> List a
rev = rv Nil
	where
	rv r Nil = r
	rv r (x :! xs) = rv (x :! r) xs

(!:) :: a -> [a] -> [a]
x !: xs = (:) x $! xs

data StrictList a = StrictList [a] deriving Show

(!!:) :: a -> StrictList a -> StrictList a
x !!: StrictList xs = StrictList $ (:) x $! xs

data Queue' a = Queue' !(StrictList a) !Int (StrictList a) !Int !(StrictList a)
	deriving Show

data Queue a = Queue !(List a) !Int (List a) !Int !(List a) deriving Show

snoc :: Queue a -> a -> Queue a
snoc (Queue w lenf f lenr r) x = check $ Queue w lenf f (lenr + 1) (x :! r)

data Empty = Empty deriving Show
instance Exception Empty

data QueueFormatError = QueueFormatError deriving Show
instance Exception QueueFormatError

head :: HasCallStack => Queue a -> a
head (Queue Nil _ _ _ _) = throw Empty
head (Queue (x :! _) _ _ _ _) = x

tail :: HasCallStack => Queue a -> Queue a
tail (Queue Nil _ _ _ _) = throw Empty
tail (Queue (_ :! w) lenf (_ :! f) lenr r) = check $ Queue w (lenf - 1) f lenr r
tail _ = throw QueueFormatError

checkw :: Queue a -> Queue a
checkw (Queue Nil lenf f lenr r) = Queue f lenf f lenr r
checkw q = q

check :: Queue a -> Queue a
check q@(Queue _ lenf f lenr r)
	| lenr <= lenf = checkw q
	| otherwise = checkw $ Queue f (lenf + lenr) (f ++! rev r) 0 Nil
