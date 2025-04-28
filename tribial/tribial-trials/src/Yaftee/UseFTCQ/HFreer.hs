{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.HFreer where

import Data.FTCQueue qualified as Q

data H h i o a = Pure a | forall x . h (H h) i o x :>>= Q.Q (H h i o) x a

instance Functor (H h i o) where
	fmap f = \case
		Pure x -> Pure $ f x
		hx :>>= q -> hx :>>= (q Q.|> (Pure . f))

instance Applicative (H h i o) where
	pure = Pure
	Pure f <*> m = f <$> m
	hx :>>= q <*> m = hx :>>= (q Q.|> (<$> m))

instance Monad (H h i o) where
	Pure x >>= f = f x
	hx :>>= q >>= f = hx :>>= (q Q.|> f)

app :: Q.Q (H h i o) a b -> a -> H h i o b
q `app` x = case Q.viewl q of
	Q.One f -> f x
	f Q.:| r -> case f x of
		Pure y -> r `app` y
		hx :>>= q' -> hx :>>= (q' Q.>< r)

comp :: (H h i o b -> H h' i' o' c) -> Q.Q (H h i o) a b -> a -> H h' i' o' c
comp = (. app) . (.)
