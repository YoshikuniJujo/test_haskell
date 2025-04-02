{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Free (Free (..)) where

data Free t a = Pure a | Join (t (Free t a))

instance Functor t => Functor (Free t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` Join tx = Join $ fmap f <$> tx

instance Functor t => Applicative (Free t) where
	pure = Pure
	Pure f <*> m = f <$> m
	Join tf <*> m = Join $ (<*> m) <$> tf

instance Functor t => Monad (Free t) where
	Pure x >>= f = f x
	Join tx >>= f = Join $ (f =<<) <$> tx
