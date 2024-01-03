{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList.TryFilter where

import GHC.TypeLits
import Data.Proxy

import HeteroList.Type

data Foo (want :: Symbol) (name :: Symbol) = Foo Int deriving Show

class FooMatch want name where
	fooMatch :: Foo want name -> Maybe (Foo want want)

instance FooMatch want want where fooMatch = Just
instance {-# OVERLAPPABLE #-} FooMatch want name where fooMatch _ = Nothing

class FooFilter want names where
	fooFilter :: PLC c (Foo want) names -> [Foo want want]

instance FooFilter want '[] where fooFilter _ = []

instance (FooMatch want name, FooFilter want names) =>
	FooFilter want (name ': names) where
	fooFilter (foo :*** foos) = maybe id (:) (fooMatch foo) $ fooFilter foos

exampleFoos :: (forall nm . FooMatch want nm) =>
--	FooMatch want "foo", FooMatch want "bar", FooMatch want "baz" ) =>
	PLC (FooMatch want) (Foo want) '["foo", "bar", "baz", "foo", "foo", "bar"]
exampleFoos =
	Foo 123 :***
	Foo 321 :***
	Foo 555 :***
	Foo 987 :***
	Foo 789 :***
	Foo 999 :***
	Nil

symbolToType :: String -> (forall (sym :: Symbol) . Proxy sym -> a) -> a
symbolToType str f = (\(SomeSymbol sym) -> f sym) $ someSymbolVal str

mkFoo :: (forall nm . FooMatch want nm) =>
	String -> Int -> (forall name . FooMatch want name => Foo want name -> a) -> a
mkFoo str n f = symbolToType str \(_ :: Proxy nm) -> f (Foo n :: Foo want nm)
