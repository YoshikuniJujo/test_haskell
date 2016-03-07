newtype Op a = Op { op :: Int -> Int -> a }

instance Functor Op where
	fmap f (Op o) = Op $ \x y -> f $ x `o` y

add :: Op Int
add = Op (+)

comp :: Op Ordering
comp = Op compare

dividable :: Op Bool
dividable = (== 0) `fmap` Op mod
