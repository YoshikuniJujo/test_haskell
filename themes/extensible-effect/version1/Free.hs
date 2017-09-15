{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Free (Free(..)) where

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
	fmap fn (Pure a) = Pure $ fn a
	fmap fn (Free fr) = Free $ fmap fn <$> fr

instance Functor f => Applicative (Free f) where
	pure = Pure
	Pure f <*> Pure x = Pure $ f x
	Pure f <*> Free fx = Free $ fmap f <$> fx
	Free ff <*> fx = Free $ (<*> fx) <$> ff

instance Functor f => Monad (Free f) where
	return = pure
	Pure a >>= f = f a
	Free m >>= f = Free $ (>>= f) <$> m
