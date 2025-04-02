{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Coyoneda where

data Coyoneda t a = forall x . Coyoneda (t x) (x -> a)

coyoneda :: t a -> Coyoneda t a
coyoneda = (`Coyoneda` id)

instance Functor (Coyoneda t) where
	f `fmap` Coyoneda tx g = Coyoneda tx $ f . g

adenoyoc :: Functor f => Coyoneda f a -> f a
adenoyoc (Coyoneda fx f) = f <$> fx

fromContext :: (forall x . t x -> b) -> Coyoneda t a -> b
fromContext f (Coyoneda tx _) = f tx

mapContext :: (forall x . t x -> t' x) -> Coyoneda t a -> Coyoneda t' a
mapContext f (Coyoneda tx g) = Coyoneda (f tx) g
