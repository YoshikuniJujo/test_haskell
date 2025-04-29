{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.HigherFreer (H(..), app, comp) where

import Control.Applicative
import Control.Monad
import Control.Monad.Freer.NonDetable qualified as NonDetable
import Control.Monad.Freer.Failable qualified as Failable
import Data.FTCQueue qualified as Q
import Data.Bool

data H h i o a = Pure a | forall x . h (H h) i o x :>>= Q.Q (H h i o) x a

instance Functor (H h i o) where
	fmap f = \case
		Pure x -> Pure $ f x; hx :>>= q -> hx :>>= (q Q.|> (Pure . f))

instance Applicative (H h i o) where
	pure = Pure
	Pure f <*> m = f <$> m; hx :>>= q <*> m = hx :>>= (q Q.|> (<$> m))

instance Monad (H h i o) where
	Pure x >>= f = f x; hx :>>= q >>= f = hx :>>= (q Q.|> f)

app :: Q.Q (H h i o) a b -> a -> H h i o b
q `app` x = case Q.viewl q of
	Q.One f -> f x
	f Q.:| r -> case f x of
		Pure y -> r `app` y; hx :>>= q' -> hx :>>= (q' Q.>< r)

comp :: (H h i o b -> H h' i' o' c) -> Q.Q (H h i o) a b -> a -> H h' i' o' c
comp = (. app) . (.)

instance NonDetable.N (h (H h) i o) => Alternative (H h i o) where
	empty = NonDetable.mz :>>= Q.singleton Pure
	m1 <|> m2 = NonDetable.mp :>>= Q.singleton (bool m1 m2)

instance NonDetable.N (h (H h) i o) => MonadPlus (H h i o) where
	mzero = empty; mplus = (<|>)

instance Failable.F (h (H h) i o) => MonadFail (H h i o) where
	fail msg = Failable.fail msg :>>= Q.singleton Pure
