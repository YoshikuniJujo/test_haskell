import Numeric

halfIntervalMethod :: (Ord f, Ord n, Fractional f, Num n) => (f -> n) -> f -> f -> f
halfIntervalMethod f a b = case (signum $ f a, signum $ f b) of
	(-1, 1) -> search f a b
	(1, -1) -> search f b a
	_ -> error $ "Values are not of opposite sign"

search :: (Ord f, Ord n, Fractional f, Num n) => (f -> n) -> f -> f -> f
search f a b
	| a `isCloseEnough` b = x
	| f x < 0 = search f x b
	| f x > 0 = search f a x
	| otherwise = x
	where
	x = (a + b) / 2

isCloseEnough :: (Ord f, Fractional f) => f -> f -> Bool
isCloseEnough x y = abs (x - y) < 0.001
