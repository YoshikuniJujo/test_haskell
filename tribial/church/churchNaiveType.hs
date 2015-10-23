toInt n = n (+ 1) 0

toChurch 0 = zero
toChurch n = s (toChurch (n - 1))

false :: a -> b -> b
false x y = y

true :: a -> b -> a
true = \x -> \y -> x

_if :: (a -> b -> c) -> a -> b -> c
_if b x y = b x y

zero :: a -> b -> b
zero = \f x -> x

s :: ((a -> b) -> c -> a) -> (a -> b) -> c -> b
s n f x = f (n f x)

one :: (a -> b) -> a -> b
one = s zero

two, three, four, five, six :: (a -> a) -> a -> a
two = s one
three = s two
four = s three
five = s four
six = s five

isZero :: ((a -> b -> c -> c) -> (d -> e -> d) -> f) -> f
isZero n = n (\x -> false) true

add m n f x = m f (n f x)
mul m n f = m (n f)
pre n f x = (n (\g h -> h (g f)) (\u -> x)) (\u -> u)

-- hoge n = _if (isZero n) one n
