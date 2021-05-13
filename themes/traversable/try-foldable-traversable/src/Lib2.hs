{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib2 where

import Control.Arrow
import Control.Monad.Fix

data Tree a = Tip | Node (Tree a) a (Tree a) deriving Show

singleton :: a -> Tree a
singleton x = Node Tip x Tip

sampleTree :: Tree Integer
sampleTree = Node
	(Node (singleton 1) 2 (singleton 3))
	4
	(Node (singleton 5) 6 (singleton 7))

instance Foldable Tree where
	foldMap = fix \k f -> \case
		Tip -> mempty
		Node l x r -> k f l <> f x <> k f  r

instance Functor Tree where
	fmap = fix \k f -> \case
		Tip -> Tip
		Node l x r -> Node (k f l) (f x) (k f r)

instance Traversable Tree where
	traverse = fix \k f -> \case
		Tip -> pure Tip
		Node l x r -> Node <$> k f l <*> f x <*> k f r

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where f `fmap` State k = State \s -> f `first` k s

instance Applicative (State s) where
	pure x = State (x ,)
	State kf <*> State kx =
		State \s -> let (f, s') = kf s; (x, s'') = kx s' in (f x, s'')

total :: Integer -> State Integer Integer
total n = State \t -> (t + n, t + n)
