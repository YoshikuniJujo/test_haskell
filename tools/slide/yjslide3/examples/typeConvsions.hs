import Data.Char

eight :: (Int -> a) -> a
eight f = f 8

some :: (a -> Int) -> a -> Bool
some num x = even (num x)

div2 :: Int -> Maybe Int
div2 n = if even n then Just $ n `div` 2 else Nothing

lower :: Int -> Maybe Char
lower n = let c = chr n in if isLower c then Just c else Nothing

(>==>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
{-
f >==> g = \x -> case f x of
	Just v -> g v
	_ -> Nothing
-}
-- f >==> g = \x -> f x >>== g
(>==>) = addA2 (>>==)
{-
f >==> g = \x -> (f x) >$> \m -> case m of
	Just v -> g v
	_ -> Nothing
-}

div2Lower :: Int -> Maybe Char
div2Lower = div2 >==> lower

(>>==) :: Maybe b -> (b -> Maybe c) -> Maybe c
m >>== g = case m of
	Just v -> g v
	_ -> Nothing

div2Lower' :: Int -> Maybe Char
div2Lower' n = div2 n >>== lower

{-

some :: ... -> (a -> b) -> ... -> (a -> c)
some ... f ... = \x -> ... (f x) ...

other :: ... -> b -> ... -> c
other ... v ... = ... v ...
	|
	V
some ... f ... = \x -> other ... (f x) ...

-}

chr' :: (a -> Int) -> (a -> Char)
-- chr' f = \x -> chr (f x)
chr' = addA chr

addA :: (b -> c) -> ((a -> b) -> (a -> c))
addA f = \g x -> f (g x)

addA2 :: (b -> c -> d) -> ((a -> b) -> c -> (a -> d))
addA2 f = \g x y -> f (g y) x

-- (a -> m b) -> (b -> m c) -> (a -> m c)
-- m b -> (b -> m c) -> m c
