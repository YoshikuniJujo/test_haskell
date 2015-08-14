data C = C { unC :: C -> C } | A C | Z

toInt :: C -> Int
toInt Z = 0
toInt (A c) = 1 + toInt c
toInt n = toInt $ n .$. C A .$. Z

toChurch :: Int -> C
toChurch 0 = zero
toChurch n = s .$. toChurch (n - 1)

(.$.) :: C -> C -> C
(.$.) = unC

false, true, _if :: C
false = C (\x -> C (\y -> y))
true = C (\x -> C (\y -> x))
_if = C (\b -> C (\x -> C (\y -> b .$. x .$. y)))

zero, one, s :: C
zero = C (\f -> C (\x -> x))
one = C (\f -> C (\x -> f .$. x))
s = C (\n -> C (\f -> C (\x -> f .$. (n .$. f .$. x))))

isZero :: C
isZero = C (\n -> n .$. (C (\x -> false)) .$. true)

add, mult, pre :: C
add = C (\m -> C (\n -> C (\f -> C (\x -> m .$. f .$. (n .$. f .$. x)))))
mult = C (\m -> C (\n -> C (\f -> m .$. (n .$. f))))
pre = C (\n -> C (\f -> C (\x ->
	(n .$. (C (\g -> C (\h -> h .$. (g .$. f))))
		.$. (C (\u -> x))) .$. (C (\u -> u)))))

hoge :: C
hoge = C (\n -> _if .$. (isZero .$. n) .$. one .$. n)
