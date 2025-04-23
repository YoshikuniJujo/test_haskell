{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQ.HFreer where

import Data.FTCQueue qualified as Q

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
