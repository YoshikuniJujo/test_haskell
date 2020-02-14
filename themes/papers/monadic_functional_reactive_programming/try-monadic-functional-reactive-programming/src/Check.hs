{-# LANGUAGE ExistentialQuantification, GADTs, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check where

import Prelude hiding (null)

class Fun f where
	fun :: (a -> b) -> f a b
	($$) :: f a b -> a -> b

data SomeFun a b where
	Id :: SomeFun a a
	F :: (a -> b) -> SomeFun a b

instance Fun SomeFun where
	fun = F
	($$) (F f) = f
	($$) Id = id

data TAList cat a b where
	Nil :: TAList cat a a
	(:^) :: cat a b -> TAList cat b c -> TAList cat a c
	(:++) :: TAList cat a b -> TAList cat b c -> TAList cat a c

data NullChecker a b where
	TR :: NullChecker a a
	FS :: NullChecker a b

class Nullable (tas :: (* -> * -> *) -> * -> * -> *) where
	null :: tas cat a b -> NullChecker a b

instance Nullable TAList where
	null Nil = TR
	null _ = FS

removeNil :: TAList cat a b -> TAList cat a b
removeNil tal0@(tal1 :++ tal2) = case null tal1 of
	TR -> tal2
	FS -> tal0
removeNil tal0 = tal0

data Freer t a = Pure a | forall x . t x :>>= (x -> Freer t a)

data It i a where
	Get :: It i [i]
