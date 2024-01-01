{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeteroList.TryFilter where

import GHC.TypeLits

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

-- exampleFoos :: PLC (FooMatch want) '["foo", "bar", "baz", "foo", "foo", "bar"]
exampleFoos :: (
	FooMatch want "foo", FooMatch want "bar", FooMatch want "baz" ) =>
	PLC (FooMatch want) (Foo want) '["foo", "bar", "baz", "foo", "foo", "bar"]
exampleFoos =
	Foo 123 :***
	Foo 321 :***
	Foo 555 :***
	Foo 987 :***
	Foo 789 :***
	Foo 999 :***
	Nil
