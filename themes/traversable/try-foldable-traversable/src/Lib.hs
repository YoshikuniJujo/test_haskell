{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow

data Tree a = Tip | Node (Tree a) a (Tree a) deriving Show

singleton :: a -> Tree a
singleton x = Node Tip x Tip

sampleTree :: Tree Integer
sampleTree = Node
	(Node (singleton 1) 2 (singleton 3))
	4
	(Node (singleton 5) 6 (singleton 7))

instance Foldable Tree where
	foldMap f = \case
		Tip -> mempty
		Node l x r -> f `foldMap` l <> f x <> f `foldMap` r

instance Functor Tree where
	fmap f = \case
		Tip -> Tip
		Node l x r -> Node (f <$> l) (f x) (f <$> r)

instance Traversable Tree where
	traverse f = \case
		Tip -> pure Tip
		Node l x r -> Node <$> f `traverse` l <*> f x <*> f `traverse` r

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where f `fmap` State k = State \s -> f `first` k s

instance Applicative (State s) where
	pure x = State (x ,)
	State kf <*> State kx =
		State \s -> let (f, s') = kf s; (x, s'') = kx s' in (f x, s'')

total :: Integer -> State Integer Integer
total n = State \t -> (t + n, t + n)
