{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TaggableFunction (Taggable) where

import Freer

data Taggable s m a b where
	Open :: Integer -> Integer -> Taggable s m a a
	Close :: Integer -> Taggable s m a a
	Fun :: (a -> m b) -> Taggable s m a b

instance Fun (Taggable s) where
	fun = Fun
	($$) (Open _ _) = pure
	($$) (Close _) = pure
	($$) (Fun f) = f

instance Tag (Taggable s) where
	open t = Open t 0
	next (Tg t m) = Open t (m + 1)
	close t = Close t
	checkOpen (Open t m) (Open t' m') | t == t' && m == m' = J (Tg t m)
	checkOpen _ _ = N
	checkClose (Tg t _) (Close t') | t == t' = T
	checkClose _ _ = F
