import Data.List

mapRaw, mapF, mapU :: (a -> b) -> [a] -> [b]
mapRaw f (x : xs) = f x : mapRaw f xs
mapRaw _ _ = []

mapF f = foldr ((:) . f) []

mapU f = unfoldr $ \l -> case l of
	x : xs -> Just (f x, xs)
	_ -> Nothing
