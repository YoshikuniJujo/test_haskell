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

data F t a = Pure a | forall x . t x :>>= Q (F t) x a

instance Functor (F t) where
	f `fmap` Pure x = Pure $ f x
	f `fmap` (tx :>>= q) = tx :>>= (q |> (Pure .f))

instance Applicative (F t) where
	pure = Pure
	Pure f <*> m = f <$> m
	tx :>>= q <*> m = tx :>>= (q |> (<$> m))

instance Monad (F t) where
	Pure x >>= f = f x
	tx :>>= q >>= f = tx :>>= (q |> f)

app :: Q (F t) a b -> a -> F t b
q `app` x = case viewl q of
	One f -> f x
	f :| r -> case f x of
		Pure y -> r `app` y
		tx :>>= q' -> tx :>>= (q' >< r)

comp :: (F t b -> F t' c) -> Q (F t) a b -> a -> F t' c
comp = (. app) . (.)

instance NonDetable.N t => Alternative (F t) where
	empty = mzero; (<|>) = mplus

instance NonDetable.N t => MonadPlus (F t) where
	mzero = NonDetable.mz :>>= singleton Pure
	m1 `mplus` m2 =
		NonDetable.mp :>>= singleton \x -> if x then m1 else m2

instance Failable.F t => MonadFail (F t) where
	fail msg = Failable.fail msg :>>= singleton Pure
