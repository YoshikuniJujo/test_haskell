{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds, ConstraintKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Bool (b) where

b :: forall {k} c (f :: k) (t :: k) tp b . (c (tp f), c (tp t)) =>
	Bool -> tp f -> tp t -> (forall (a :: k) . c (tp a) => tp a -> b) -> b
b False f _ fn  = fn f
b True _ t fn = fn t
