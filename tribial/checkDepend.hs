{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

type family Foo t

data FooInt = FooInt
data FooBool = FooBool

type instance Foo FooInt = Int
type instance Foo FooBool = Bool

class Bar a where bar :: a
instance Bar Int where bar = 0
instance Bar Bool where bar = False

foo :: String -> (forall x . (Show (Foo x), Bar (Foo x)) => x -> Foo x) -> String
foo "int" f = show (f FooInt)
foo "bool" f = show (f FooBool)
