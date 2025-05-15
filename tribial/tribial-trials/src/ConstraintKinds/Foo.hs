{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ConstraintKinds.Foo where

type Stringy a = (Read a, Show a)

foo :: (Stringy a, Eq a) => a -> (String, String -> a)
foo x = (show x, read)

type Foo = Int ': Char ': Bool ': '[]
type Bar = Float ': () ': '[]

type family Append xs ys where
	Append '[] ys = ys
	Append (x ': xs) ys = x ': Append xs ys

type Baz = Foo `Append` Bar
