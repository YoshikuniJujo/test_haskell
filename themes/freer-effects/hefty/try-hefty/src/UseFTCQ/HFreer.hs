{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQ.HFreer where

import Control.Applicative
import Control.Monad
import Control.Monad.Freer.NonDetable qualified as NonDetable
import Control.Monad.Freer.Failable qualified as Failable
import Data.FTCQueue qualified as Q
import Data.Bool

data H h a = Pure a | forall x . h (H h) x :>>= Q.Q (H h) x a

instance Functor (H h) where
	fmap f = \case
		Pure x -> Pure $ f x
		hx :>>= q -> hx :>>= (q Q.|> (Pure . f))

instance Applicative (H h) where
	pure = Pure
	Pure f <*> m = f <$> m
	hx :>>= q <*> m = hx :>>= (q Q.|> (<$> m))

instance Monad (H h) where
	Pure x >>= f = f x
	hx :>>= q >>= f = hx :>>= (q Q.|> f)

app :: Q.Q (H h) a b -> a -> H h b
q `app` x = case Q.viewl q of
	Q.One f -> f x
	f Q.:| r -> case f x of
		Pure y -> r `app` y
		hx :>>= q' -> hx :>>= (q' Q.>< r)

comp :: (H h b -> H h' c) -> Q.Q (H h) a b -> a -> H h' c
comp = (. app) . (.)

instance NonDetable.N (h (H h)) => Alternative (H h) where
	empty = mzero; (<|>) = mplus

instance NonDetable.N (h (H h)) => MonadPlus (H h) where
	mzero = NonDetable.mz :>>= Q.singleton Pure
	m1 `mplus` m2 = NonDetable.mp :>>= Q.singleton (bool m1 m2)

instance Failable.F (h (H h)) => MonadFail (H h) where
	fail msg = Failable.fail msg :>>= Q.singleton Pure
