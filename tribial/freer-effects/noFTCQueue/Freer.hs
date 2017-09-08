{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Freer (Freer(..)) where

import Control.Monad

data Freer f a = Pure a | forall x . Join (f x) (x -> Freer f a)

instance Functor (Freer f) where
	fmap = (=<<) . (return .)

instance Applicative (Freer f) where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad (Freer f) where
	return = Pure
	Pure x >>= f = f x
	Join fa k >>= f = Join fa $ k >=> f
