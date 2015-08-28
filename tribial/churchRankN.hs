{-# LANGUAGE RankNTypes #-}

false x y = y
true = \x -> \y -> x

_if :: (forall a b c . a -> b -> c) -> a -> b -> c
_if b x y = b x y

zero = \f x -> x

one :: (forall a b . a -> b) -> (forall a . a) -> b
-- one :: (forall a . a) -> (forall a . a) -> a
one f x = f x

s n f x = f (n f x)

isZero :: (forall a .
	(forall a b c . a -> b -> c -> c) ->
	(forall a b . a -> b -> a) -> a) -> b
-- isZero :: (forall a b c . a -> b -> c) -> b
-- isZero :: ((forall a . a) -> (forall b . b) -> c) -> b
isZero n = n (\x -> false) true
add m n f x = m f (n f x)
mult m n f = m (n f)

-- toInt :: (forall a . a) -> Int
toInt n = n (+ 1) 0
toChurch 0 = zero
toChurch n = s (toChurch (n - 1))
pre n f x = (n (\g h -> h (g f)) (\u -> x)) (\u -> u)

-- hoge :: (forall a b c . a -> b -> c) -> a
hoge :: (forall a . a) -> a
hoge n = _if (isZero n) one n
