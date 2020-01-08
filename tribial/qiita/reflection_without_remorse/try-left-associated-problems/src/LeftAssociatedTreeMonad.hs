{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LeftAssociatedTreeMonad where

import Control.Monad

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

(<--) :: Tree a -> (a -> Tree b) -> Tree b
Leaf x <-- f = f x
Node l r <-- f = Node (l <-- f) (r <-- f)

instance Functor Tree where
	f `fmap` Leaf x = Leaf $ f x
	f `fmap` Node l r = Node (f <$> l) (f <$> r)

instance Applicative Tree where
	pure = Leaf
	mf <*> mx = mf <-- \f -> mx <-- \x -> pure $ f x

instance Monad Tree where
	(>>=) = (<--)

sampleTree0, sampleTree1 :: Tree ()
sampleTree0 = Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))
sampleTree1 = Node (Leaf ()) (Leaf ())

sampleFun0, sampleFun1 :: () -> Tree ()
sampleFun0 = const sampleTree0
sampleFun1 = const sampleTree1

leftMost :: Tree a -> a
leftMost (Leaf x) = x
leftMost (Node l _) = leftMost l

countNode :: Tree a -> Int
countNode (Leaf _) = 0
countNode (Node (Leaf _) (Leaf _)) = 1
countNode (Node l r) = 1 + countNode l + countNode r

sample0L, sample0R, sample1L, sample1R :: () -> Tree ()
sample0L = foldl (>=>) pure $ replicate 11 sampleFun0
sample0R = foldr (>=>) pure $ replicate 11 sampleFun0
sample1L = foldl (>=>) pure $ replicate 23 sampleFun1
sample1R = foldr (>=>) pure $ replicate 23 sampleFun1
