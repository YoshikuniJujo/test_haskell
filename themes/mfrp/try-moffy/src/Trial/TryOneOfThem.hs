{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Trial.TryOneOfThem where

import Data.Type.Set
import Data.OneOfThem

numbered [t| Bool |]
numbered [t| () |]
numbered [t| Char |]

foo, bar, baz :: OneOfThem (Bool :- () :- Char :- 'Nil)
foo = expand $ Singleton True
bar = expand $ Singleton ()
baz = expand $ Singleton 'c'

f :: OneOfThemFun (Bool :- () :- Char :- 'Nil) String
f = (show :: Bool -> String) >-- (show :: () -> String) >-- SingletonFun (show :: Char -> String)

fooBarBaz :: [OneOfThem (Bool :- () :- Char :- 'Nil)]
fooBarBaz = 'c' >- True >- () >- 'c' >- ([] :: [OneOfThem 'Nil])
