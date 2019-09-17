{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data family Foo x

data instance Foo Int = FooInt Bool Char
data instance Foo Char = FooChar Double Integer

class UseFoo a where
	useFoo :: a -> Foo a -> String

instance UseFoo Int where
	useFoo x (FooInt y z) = show x ++ " " ++ show y ++ " " ++ show z

instance UseFoo Char where
	useFoo x (FooChar y z) = show x ++ " " ++ show y ++ " " ++ show z
