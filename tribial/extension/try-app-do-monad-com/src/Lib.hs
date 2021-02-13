{-# LANGUAGE ApplicativeDo, MonadComprehensions #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

newtype Foo a = Foo a deriving Show

instance Functor Foo where f `fmap` Foo x = Foo $ f x

instance Applicative Foo where
	pure = Foo
	Foo f <*> Foo x = Foo $ f x

sample :: Foo Integer
sample = [ x | x <- pure 123 ]
