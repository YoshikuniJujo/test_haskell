{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RankNTypes where

import Data.Kind
import Data.Proxy

foo :: (forall k (a :: k) . TypeInt a => Int) -> Int
foo x = x @Type @Int

{-
zero0 :: Int
zero0 = foo int
-}

class TypeInt (a :: k) where int :: Int

instance TypeInt Int where int = 0
instance TypeInt Double where int = 1

bar :: (forall k (a :: k) . TypeInt a => Proxy a -> Int) -> Int
bar f = f @Type @Int Proxy

baz :: (forall k (a :: k) . TypeInt a => Proxy a -> Int) -> Int
baz f = f @Type @Double Proxy

int' :: forall a . TypeInt a => Proxy a -> Int
int' _ = int @_ @a

zero, one :: Int
zero = bar int'
one = baz int'

int'' :: forall a . TypeInt a => () -> Int
int'' _ = int @_ @a

qux :: (forall k (a :: k) . TypeInt a => () -> Int) -> Int
qux f = f @_ @Int ()

{-
zero1 :: Int
zero1 = qux int''
-}
