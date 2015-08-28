data C a = C { unC :: C a -> C a } | A (C a) | Z

toInt :: C a -> Int
toInt Z = 0
toInt (A c) = 1 + toInt c
toInt n = toInt (unC (unC n (C A)) Z)

toChurch :: Int -> C a
toChurch 0 = zero
toChurch n = unC s (toChurch (n - 1))

(.$.) :: C a -> C a -> C a
(.$.) = unC

false, true, _if :: C a
false = C (\x -> C (\y -> y))
true = C (\x -> C (\y -> x))
_if = C (\b -> C (\x -> C (\y -> b .$. x .$. y)))

zero, one, s :: C a
zero = C (\f -> C (\x -> x))
one = C (\f -> C (\x -> f .$. x))
s = C (\n -> C (\f -> C (\x -> f .$. (n .$. f .$. x))))

isZero :: C a
isZero = C (\n -> n .$. (C (\x -> false)) .$. true)

add, mult, pre :: C a
add = C (\m -> C (\n -> C (\f -> C (\x -> m .$. f .$. (n .$. f .$. x)))))
mult = C (\m -> C (\n -> C (\f -> m .$. (n .$. f))))
pre = C (\n -> C (\f -> C (\x ->
	(n .$. (C (\g -> C (\h -> h .$. (g .$. f))))
		.$. (C (\u -> x))) .$. (C (\u -> u)))))

hoge :: C a
hoge = C (\n -> _if .$. (isZero .$. n) .$. one .$. n)
