import Control.Monad

pointsC, pointsD :: [(Integer, Integer)]
-- pointsC = [ (x, y) | x <- [0 .. 10], y <- [0 .. 10], x ^ 2 + y ^ 2 <= 100 ]
pointsC = [ (x, y) |
	x <- [0 .. 10],
	y <- [0 .. 10],
	_ <- if x ^ 2 + y ^ 2 <= 100 then [()] else [] ]

pointsD = do
	x <- [0 .. 10]
	y <- [0 .. 10]
	guard $ x ^ 2 + y ^ 2 <= 100
	return (x, y)

fcm = flip concatMap

pointsM =
	(`concatMap` [0 .. 10]) $ \x ->
	(`concatMap` [0 .. 10]) $ \y ->
	(`concatMap` (if x ^ 2 + y ^ 2 <= 100 then [()] else [])) $ \_ ->
	[(x, y)]
