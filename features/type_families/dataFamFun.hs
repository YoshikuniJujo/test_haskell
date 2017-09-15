{-# LANGUAGE TypeFamilies, GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data family Foo x

data instance Foo Int = FooInt Bool Int

fun :: Foo Int -> Bool
fun (FooInt b _) = b

{-
fun2 :: Foo a -> a
fun2 (FooInt _ i) = i
-}

data Bar x where
	BarInt :: Bool -> Int -> Bar Int

fun2 :: Bar a -> a
fun2 (BarInt _ i) = i

type Fantom a = Int

fun3 :: Fantom a -> Fantom a
fun3 = id

type family Baz a

type instance Baz Int = Int

type instance Baz Double = Int

fun4 :: a -> Baz a -> Baz a
fun4 _ = id
