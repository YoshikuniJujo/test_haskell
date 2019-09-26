{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Coyoneda where

import Control.Arrow

import FunctionTree

data Coyoneda t a = forall x . Coyoneda (t x) (x -> a)

coyoneda :: t a -> Coyoneda t a
coyoneda = (`Coyoneda` id)

adenoyoc :: Functor f => Coyoneda f a -> f a
adenoyoc (Coyoneda fx f) = f <$> fx

instance Functor (Coyoneda t) where
	f `fmap` Coyoneda tx g = Coyoneda tx $ g >>> f

data CoyonedaFt t a = forall x . CoyonedaFt (t x) (FunctionTree x a)

coyonedaFt :: t a -> CoyonedaFt t a
coyonedaFt = (`CoyonedaFt` tsingleton id)

adenoyocFt :: Functor f => CoyonedaFt f a -> f a
adenoyocFt (CoyonedaFt fx f) = (f `ftApp`) <$> fx

instance Functor (CoyonedaFt t) where
	f `fmap` CoyonedaFt tx g = CoyonedaFt tx $ g |> f
