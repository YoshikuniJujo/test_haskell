-- https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism

{-# LANGUAGE RankNTypes #-}

f :: (forall a . a -> a) -> Int
f i = i 85

g :: Bool -> b -> b
g _ = id

test1 :: Bool -> Int
test1 x = f (g x)

-- test2 :: Bool -> Int
-- test2 = f . g

{-

test1: OK
test2: Couldn't match type 'b0 -> b0' with 'forall a . a -> a'

-}
