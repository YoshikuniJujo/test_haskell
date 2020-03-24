{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Injective where

type family Foo a = r | r -> a

type family Bar a = r | r -> a where
	Bar Double = Integer
	Bar Bool = Int
