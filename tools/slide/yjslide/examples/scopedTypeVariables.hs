{-# LANGUAGE ScopedTypeVariables #-}

twice :: forall a . [a] -> [a]
twice xs = ys
	where
	ys :: [a]
	ys = xs ++ xs

one :: forall a . (Num a, Show a) => a -> String
one x = show (1 :: a)

{-
one' :: (Num a, Show a) => a -> String
one' x = show (1 :: a)
-}

one' (x :: a) = show (1 :: a)

one'' :: (Num a, Show a) => a -> String
one'' = \(x :: a) -> show (1 :: a)

some (x :: Maybe a) = show (1 :: a)

{-
sortImage f :: ([a] -> [a]) = sortBy cmp
	where
	cmp :: a -> a -> Ordering
	cmp x y = compare (f x) (f y)
	-}

withOne :: (Num a, Show a) => (a -> a) -> String
withOne f = show $ f 1
