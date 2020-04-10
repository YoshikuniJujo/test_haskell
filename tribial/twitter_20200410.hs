{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

data F a = F a

hoge :: a -> F a
hoge = F

data B = B

class A a where
	foo :: B -> a

instance (A a) => A (F a) where
	foo x = hoge (foo x :: a) :: F a
