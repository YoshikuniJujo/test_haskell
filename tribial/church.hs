data C a = C { unC :: C a -> C a } | A (C a) | Z

false, true, _if :: C a
false = C $ \x -> C $ \y -> y
true = C $ \x -> C $ \y -> x
_if = C $ \b -> C $ \x -> C $ \y -> unC (unC b x) y

zero, one :: C a
zero = C $ \f -> C $ \x -> x
one = C $ \f -> C $ \x -> unC f x

isZero :: C a
isZero = C $ \n -> unC (unC n (C $ \x -> false)) true

s :: C a
s = C $ \n -> C $ \f -> C $ \x -> unC f (unC (unC n f) x)

add :: C a
add = C $ \m -> C $ \n -> C $ \f -> C $ \x -> m `unC` f `unC` (n `unC` f `unC` x)

mult = C $ \m -> C $ \n -> C $ \f -> m `unC` (n `unC` f)
pre n f x = (n (\g h -> h (g f)) (\u -> x)) (\u -> u)

hoge :: C a
hoge = C $ \n -> _if `unC` (isZero `unC` n) `unC` one `unC` n

toInt :: C a -> Int
toInt Z = 0
toInt (A c) = 1 + toInt c
toInt n = toInt (unC (unC n (C A)) Z)

toChurch :: Int -> C a
toChurch 0 = zero
toChurch n = unC s (toChurch (n - 1))
