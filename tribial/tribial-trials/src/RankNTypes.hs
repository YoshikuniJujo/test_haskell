{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RankNTypes where

import Data.Proxy

foo :: (forall a . TypeInt a => Int) -> Int
foo x = x @Int

{-
zero0 :: Int
zero0 = foo int
-}

class TypeInt a where int :: Int

instance TypeInt Int where int = 0
instance TypeInt Double where int = 1

bar :: (forall a . TypeInt a => Proxy a -> Int) -> Int
bar f = f @Int Proxy

baz :: (forall a . TypeInt a => Proxy a -> Int) -> Int
baz f = f @Double Proxy

int' :: forall a . TypeInt a => Proxy a -> Int
int' _ = int @a

zero, one :: Int
zero = bar int'
one = baz int'

int'' :: forall a . TypeInt a => () -> Int
int'' _ = int @a

qux :: (forall a . TypeInt a => () -> Int) -> Int
qux f = f @Int ()

{-
zero1 :: Int
zero1 = qux int''
-}
