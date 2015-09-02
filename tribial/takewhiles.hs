import Data.List

takeWhileRaw, takeWhileF :: (a -> Bool) -> [a] -> [a]
takeWhileRaw p (x : xs) | p x = x : takeWhileRaw p xs
takeWhileRaw _ _ = []

takeWhileF p = foldr (\x -> if p x then (x :) else const []) []

takeWhileU p = unfoldr $ \l -> case l of
	x : xs | p x -> Just (x, xs)
	_ -> Nothing

dropWhileRaw, dropWhileF :: (a -> Bool) -> [a] -> [a]
dropWhileRaw p (x : xs) | p x = dropWhileRaw p xs
dropWhileRaw _ xs = xs

dropWhileF p xs = foldr f (const []) xs True where
	f x g True
		| p x = g True
		| otherwise = x : g False
	f x g False = x : g False
