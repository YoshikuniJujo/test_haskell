{-# LANGUAGE TypeFamilies #-}

type family Foo x

type instance Foo Integer = Bool
type instance Foo Double = Char
