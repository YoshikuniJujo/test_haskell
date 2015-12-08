invert, invert' :: Integral n => (n -> n -> n) -> n -> [(n, n)]
invert f z = [ (x, y) | x <- [0 .. z], y <- [0 .. z], f x y == z ]
invert' f z = [ (x, y) | x <- [0 .. z], y <- [0 .. z - x], f x y == z ]

mul :: Integral n => n -> n -> n
mul x y = (x + 1) * (y + 1)

find (u, v) f z = [ (x, y) |
	x <- [u .. z], y <- [v, v - 1 .. 0], f x y == z ]

find' (u, v) f z
	| u > z || v < 0 = []
	| z' < z = find (u + 1, v) f z
	| z' == z = (u, v) : find (u + 1, v - 1) f z
	| z' > z = find (u, v - 1) f z
	where
	z' = f u v

invert'' f z = find' (0, z) f z

bsearch g (a, b) z
	| a + 1 == b = a
	| g m <= z = bsearch g (m, b) z
	| otherwise = bsearch g (a, m) z
	where m = (a + b) `div` 2
