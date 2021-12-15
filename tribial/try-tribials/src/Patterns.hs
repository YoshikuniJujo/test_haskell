{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Patterns where

newtype Foo = Foo Int deriving Show

pattern FooOne :: Foo
pattern FooOne <- Foo 1 where
	FooOne = Foo 1
