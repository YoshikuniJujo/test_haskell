{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

class Foo a

instance Foo a => Foo [a]

foo :: Foo [a] => a -> a
foo = id

class Bar a

instance Bar String

instance {-# OVERLAPPABLE #-} Bar a => Bar [a]

bar :: Bar [a] => a -> a
bar = id

class Baz a b

instance Baz a a

instance {-# OVERLAPPABLE #-} (Foo a, Foo b) => Baz a b

baz :: Baz a b => a -> b -> (a, b)
baz = (,)
