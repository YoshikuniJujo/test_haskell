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
