{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQueue.Freer (Freer(..), Q, singleton, app, comp) where

import Control.Applicative
import Control.Monad
import UseFTCQueue.FTCQueue
import NonDetable

data Freer t a = Pure a | forall x . Bind (t x) (Q (Freer t) x a)

instance Functor (Freer t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (tx `Bind` q) = tx `Bind` (q |> (Pure .f))

instance Applicative (Freer t) where
	pure = Pure
	Pure f <*> m = f <$> m
	tx `Bind` q <*> m = tx `Bind` (q |> (<$> m))

instance Monad (Freer t) where
	Pure x >>= f = f x
	tx `Bind` q >>= f = tx `Bind` (q |> f)

app :: Q (Freer t) a b -> a -> Freer t b
q `app` x = case viewl q of
	One f -> f x
	f :| r -> case f x of
		Pure y -> r `app` y
		tx `Bind` q' -> tx `Bind` (q' >< r)

comp :: (Freer t b -> Freer t' c) -> Q (Freer t) a b -> a -> Freer t' c
comp = (. app) . (.)

instance NonDetable t => Alternative (Freer t) where
	empty = mzero; (<|>) = mplus

instance NonDetable t => MonadPlus (Freer t) where
	mzero = mz `Bind` singleton Pure
	m1 `mplus` m2 = mp `Bind` singleton \x -> if x then m1 else m2
