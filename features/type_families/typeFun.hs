{-# LANGUAGE TypeFamilies #-}

type family Foo x

type instance Foo Bool = Int

type instance Foo Int = Double

some :: a -> Foo a -> Foo a
some _ = id
