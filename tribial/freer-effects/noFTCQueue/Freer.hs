{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (Freer(..)) where

import Control.Monad

{-
data FFree f a = forall b . FMap (b -> a) (f b)

instance Functor (FFree f) where
	fmap f (FMap g a) = FMap (f . g) a

data Free f a
	= Pure a
	| Join (f (Free f a))

type Freer f = Free (FFree f)
-}

data Freer f a = Pure a | forall b . Join (f b) (b -> Freer f a)

instance Functor (Freer f) where
	fmap = (=<<) . (return .)

instance Applicative (Freer f) where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad (Freer f) where
	return = Pure
	Pure x >>= f = f x
	Join fa k >>= f = Join fa $ k >=> f
