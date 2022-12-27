{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Identity (Identity(..)) where

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where fmap = (Identity .) . (. runIdentity)

instance Applicative Identity where
	pure = Identity; Identity f <*> Identity x = Identity $ f x

instance Monad Identity where Identity x >>= f = f x
