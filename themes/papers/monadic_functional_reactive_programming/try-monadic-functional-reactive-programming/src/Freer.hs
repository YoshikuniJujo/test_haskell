{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer where

import Sequence

data Freer s sq (f :: (* -> *) -> * -> * -> *) t a = Pure a | forall x . t x :>>= sq (f (Freer s sq f t)) x a

class Fun f where fun :: (a -> m b) -> f m a b

instance (Sequence sq, Fun f) => Functor (Freer s sq f t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (m :>>= k) = m :>>= (k |> fun (Pure . f))

instance (Sequence sq, Fun f) => Applicative (Freer s sq f t) where
	pure = Pure
	Pure f <*> mx = f <$> mx
	(m :>>= k) <*> mx = m :>>= (k |> fun (<$> mx))

instance (Sequence sq, Fun f) => Monad (Freer s sq f t) where
	Pure x >>= f = f x
	(m :>>= k) >>= f = m :>>= (k |> fun f)

(>>>=) :: (Sequence sq, Fun f) => t a -> (a -> Freer s sq f t b) -> Freer s sq f t b
m >>>= f = m :>>= singleton (fun f)

newtype Count s a = Count { unCount :: Integer -> (a, Integer) }
