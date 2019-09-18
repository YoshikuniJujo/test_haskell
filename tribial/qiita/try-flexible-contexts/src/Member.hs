{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, MultiParamTypeClasses,
	FlexibleInstances, FlexibleContexts, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Member where

class Member a (as :: [*])

instance Member t (t ': ts)

instance {-# OVERLAPPABLE #-} Member t ts => Member t (_t' ': ts)

newtype Foo (as :: [*]) = Foo Int

foo :: Member Int as => Foo as -> Int
foo (Foo x) = x

type family Bar (as :: [*]) = r | r -> as

type instance Bar '[Double, Int, Double] = Int

type instance Bar '[Double, Char, Bool] = Double

bar :: Member Int as => Bar as -> Int
bar _ = 123
