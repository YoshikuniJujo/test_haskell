{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Monoid

newtype Flip t a b = Flip { unflip :: t b a } deriving Show

data RetList a b = Ret b | a :| RetList a b deriving Show

infixr 5 :|

(.++) :: Semigroup b => RetList a b -> RetList a b -> RetList a b
Ret x .++ Ret y = Ret $ x <> y
r@(Ret _) .++ (y :| lst) = y :| r .++ lst
(x :| lst1) .++ lst2 = x :| (lst1 .++ lst2)

instance Functor (RetList a) where
	f `fmap` Ret x = Ret $ f x
	f `fmap` (x :| lst) = x :| (f `fmap` lst)

instance Applicative (RetList a) where
	pure = Ret
	Ret f <*> mx = f <$> mx
	(x :| mf) <*> mx = x :| (mf <*> mx)

instance Monad (RetList a) where
	Ret x >>= f = f x
	(x :| m) >>= f = x :| (m >>= f)

instance Functor (Flip RetList b) where
	_ `fmap` Flip (Ret x) = Flip $ Ret x
	f `fmap` Flip (x :| lst) = Flip $ f x :| unflip (f `fmap` Flip lst)

instance Monoid b => Applicative (Flip RetList b) where
	pure = Flip . (:| Ret mempty)
	Flip (Ret _) <*> _ = Flip $ Ret mempty
	Flip (f :| mf) <*> mx = Flip $ unflip (f <$> mx) .++ unflip (Flip mf <*> mx)

instance Monoid b => Monad (Flip RetList b) where
	Flip (Ret x) >>= _ = Flip $ Ret x
	Flip (x :| m) >>= f = Flip $ unflip (f x) .++ unflip (Flip m >>= f)

sample1, sample2 :: RetList Int (Sum Double)
sample1 = 1 :| 2 :| 3 :| Ret 123
sample2 = 1 :| 1 :| 2 :| 3 :| 5 :| Ret 321
