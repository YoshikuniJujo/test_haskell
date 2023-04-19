{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds, ConstraintKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.Bool (b) where

b :: forall {k} c (f :: k) (t :: k) tp b . (c (tp f), c (tp t)) =>
	tp f -> tp t -> Bool -> (forall (a :: k) . c (tp a) => tp a -> b) -> b
b f _ False fn  = fn f
b _ t True fn = fn t
