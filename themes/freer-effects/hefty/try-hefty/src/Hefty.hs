{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hefty where

import Data.Kind

-- data Freer t a = Pure a | forall x . t x :>>= (x -> Freer t a)

data Hefty h a = Pure a | forall x . h (Hefty h) x :>>= (x -> Hefty h a)

data Throw e a where
	Throw :: e -> Throw e a

data Catch e f a where
	Catch :: f a -> (e -> f a) -> Catch e f a

newtype Foo (t :: Type -> Type) (f :: Type -> Type) a = Foo (t a)

bar :: Hefty (Foo (Throw Integer)) Integer
bar = Foo (Throw 123) :>>= const (Pure 456)

foo = (Pure 123 `Catch` const (Pure 456)) :>>= const (Pure 789)

convertHefty :: (forall x . h (Hefty h) x -> g (Hefty g) x) -> Hefty h a -> Hefty g a
convertHefty c = \case
	Pure x -> Pure x
	hhx :>>= k -> c hhx :>>= \y -> convertHefty c (k y)
