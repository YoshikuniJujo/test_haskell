data MyInteger = MyInteger Bool [Bool]

toI :: MyInteger -> Integer
toI (MyInteger s bs) = if s then negate $ ti bs + 1 else ti bs
	where
	ti [] = 0
	ti (b : bs) = (if b then 1 else 0) + 2 * ti bs

instance Show MyInteger where show = show . toI

add :: Bool -> MyInteger -> MyInteger -> MyInteger
add a (MyInteger s (b : bs)) (MyInteger t (c : cs))
