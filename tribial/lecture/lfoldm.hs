sub :: Integer -> Integer -> Maybe Integer
x `sub` y
	| x >= y = Just $ x - y
	| otherwise = Nothing

lfoldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
lfoldM f v (x : xs) = f v x >>= \v' -> lfoldM f v' xs
lfoldM _ v _ = return v
