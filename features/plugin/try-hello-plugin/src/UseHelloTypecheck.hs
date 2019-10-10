{-# LANGUAGE ScopedTypeVariables, GADTs, DataKinds, TypeOperators, KindSignatures, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=HelloTypecheck.Plugin -fplugin-opt=DoNothing.Plugin:abc,def
	-fplugin-opt=HelloTypecheck.Plugin:ghi #-}

module UseHelloTypecheck where

import GHC.TypeLits

{-
data NList n a where
	Nil :: NList 0 a
	(:.) :: a -> NList n a -> NList (n + 1) a
	-}

{-
push :: a -> NList n a -> NList (n + 1) a
push = (:.)

foo :: m <= 123 => Int
foo = 54321
-}

newtype NInt (n :: Nat) = NInt Int

bar :: m ~ ((m + 1) - 1) => NInt m
bar = NInt 123

-- append :: NList n a -> NList m a -> NList (n + m + 321) a
-- append Nil ys = ys
-- append (x :. xs) ys = x :. append (xs :: NList (n - 1) a) ys

infixr 5 :::

data List :: Nat -> * -> * where
	Nil :: List 0 a
	(:::) :: a -> List length a -> List (length + 1) a

deriving instance Show a => Show (List n a)

head_ :: List (n + 1) a -> a
head_ (a ::: _) = a

tail_ :: List (n + 1) a -> List n a
tail_ (_ ::: as) = as
