data C a = C { unC :: C a -> C a } | X

false x y = y
true = \x -> \y -> x
_if b x y = b x y

zero = \f x -> x
one f x = f x

s n f x = f (n f x)
isZero n = n (\x -> false) true
add m n f x = m f (n f x)
mult m n f = m (n f)

-- toInt :: C Int -> Int
-- toInt n = unC n (C (+ 1)) 0

toChurch 0 = zero
toChurch n = s (toChurch (n - 1))
pre n f x = (n (\g h -> h (g f)) (\u -> x)) (\u -> u)

-- hoge n = _if (isZero n) one n
