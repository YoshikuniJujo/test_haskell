toInt n = n (+ 1) 0
toChurch 0 = zero
toChurch n = s . toChurch $ n - 1

false x y = y
true = \x -> \y -> x
_if b x y = b x y

zero = \f x -> x
s n f x = f $ n f x
one = s zero
two = s one
three = s two
four = s three
five = s four
six = s five

isZero n = n (\x -> false) true
add m n f x = m f $ n f x
mult m n f = m $ n f
pre n f x = n (\g h -> h $ g f) (\u -> x) (\u -> u)

zeroToOne n = _if (isZero n) one zero
-- zeroToOne n = _if (isZero n) one n
