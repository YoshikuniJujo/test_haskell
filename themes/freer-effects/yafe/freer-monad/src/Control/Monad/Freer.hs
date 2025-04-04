{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer (F(..), app, comp) where

import Control.Applicative
import Control.Monad
import Data.FTCQueue
import Control.Monad.Freer.NonDetable qualified as NonDetable
import Control.Monad.Freer.Failable qualified as Failable

data F t a = Pure a | forall x . Bind (t x) (Q (F t) x a)

instance Functor (F t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (tx `Bind` q) = tx `Bind` (q |> (Pure .f))

instance Applicative (F t) where
	pure = Pure
	Pure f <*> m = f <$> m
	tx `Bind` q <*> m = tx `Bind` (q |> (<$> m))

instance Monad (F t) where
	Pure x >>= f = f x
	tx `Bind` q >>= f = tx `Bind` (q |> f)

app :: Q (F t) a b -> a -> F t b
q `app` x = case viewl q of
	One f -> f x
	f :| r -> case f x of
		Pure y -> r `app` y
		tx `Bind` q' -> tx `Bind` (q' >< r)

comp :: (F t b -> F t' c) -> Q (F t) a b -> a -> F t' c
comp = (. app) . (.)

instance NonDetable.N t => Alternative (F t) where
	empty = mzero; (<|>) = mplus

instance NonDetable.N t => MonadPlus (F t) where
	mzero = NonDetable.mz `Bind` singleton Pure
	m1 `mplus` m2 =
		NonDetable.mp `Bind` singleton \x -> if x then m1 else m2

instance Failable.F t => MonadFail (F t) where
	fail msg = Failable.fail msg `Bind` singleton Pure
