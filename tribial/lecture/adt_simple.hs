{-# LANGUAGE RankNTypes #-}

newtype Some = Some Int deriving Show

some :: Some -> Int
some (Some n) = n

f :: Some -> ()
f (Some _) = ()

data Other = Other Int deriving Show

other :: Other -> Int
other (Other n) = n

g :: Other -> ()
g (Other _) = ()

h :: Int -> ()
h _ = ()

tup :: a -> b -> ((a -> b -> Either a b) -> Either a b)
tup x y f = f x y

ft :: a -> b -> Either a b
ft x _ = Left x

sd :: a -> b -> Either a b
sd _ y = Right y

newtype List a =
	List ((a -> List a -> Either a (List a)) -> Maybe (Either a (List a)))

instance Show a => Show (List a) where
	show (List l) = case (l ft, l sd) of
		(Just (Left x), Just (Right xs)) -> show x ++ " : " ++ show xs
		_ -> "[]"

empty :: List a
empty = List $ const Nothing

list :: a -> List a -> List a
list x xs = List $ \f -> Just $ f x xs

infixr 9 `list`
