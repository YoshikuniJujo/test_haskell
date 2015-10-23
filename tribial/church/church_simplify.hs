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
false = C $ \x -> C id
true = C $ \x -> C $ const x
_if = C $ \b -> C $ \x -> C $ \y -> b .$. x .$. y

zero, s :: C
zero = C $ \f -> C id
s = C $ \n -> C $ \f -> C $ \x -> f .$. (n .$. f .$. x)
