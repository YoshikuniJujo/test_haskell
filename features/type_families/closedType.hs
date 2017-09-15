{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family Foo x where
	Foo Int = Bool
	Foo Bool = Char

foo :: x -> Foo x -> Foo x
foo _ fx = fx

type family Bar x

type instance Bar Int = Bool
type instance Bar Bool = Char

bar :: x -> Bar x -> Bar x
bar _ bx = bx
