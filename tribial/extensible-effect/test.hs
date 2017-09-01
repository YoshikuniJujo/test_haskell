{-# LANGUAGE RankNTypes #-}

data Val a = Val a deriving Show

data Foo a = Foo (forall b . (a -> Val b) -> Val b)

admin :: Foo a -> Val a
admin (Foo m) = m Val

data Hoge a = Hoge (forall f . Applicative f => f a)
