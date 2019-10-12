{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=PlusOne.Plugin -fplugin=Foo.Plugin #-}

module TryPlusOne where

import GHC.TypeLits

infixr 5 :::

data List :: Nat -> * -> * where
	Nil :: List 0 a
	(:::) :: a -> List length a -> List (length + 1) a

deriving instance Show a => Show (List n a)

tail_ :: List (n + 1) a -> List n a
tail_ (_ ::: as) = as

class Foo a

foo :: Foo a => a -> a
foo = id

three :: Int
three = foo 3
