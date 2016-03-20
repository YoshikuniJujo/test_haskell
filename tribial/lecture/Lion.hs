{-# LANGUAGE MonadComprehensions #-}

module Lion (Lion, Caged, lion, feed, play) where

import Control.Applicative

data Lion = Lion Name State deriving Show

type Name = String
data State = Hungry | Normal | Full deriving Show

newtype Caged a = Caged a deriving Show

instance Functor Caged where
	fmap = (=<<) . (return .)

instance Applicative Caged where
	pure = return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad Caged where
	return = Caged
	Caged x >>= f = f x

lion :: Name -> Caged Lion
lion n = Caged $ Lion n Hungry

feed, play :: Lion -> Lion
feed (Lion n Hungry) = Lion n Normal
feed (Lion n _) = Lion n Full

play (Lion n Full) = Lion n Normal
play (Lion n _) = Lion n Hungry
