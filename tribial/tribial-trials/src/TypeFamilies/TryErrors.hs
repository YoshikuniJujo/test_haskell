{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeFamilies.TryErrors where

type family Foo a

type instance Foo Int = Char

foo :: a -> Foo a -> Foo a
foo _ = id

class Bar a where type Baz a

instance Bar Int where type Baz Int = Char

bar :: Bar a => a -> Baz a -> Baz a
bar _ = id

x :: Foo Double
x = undefined
