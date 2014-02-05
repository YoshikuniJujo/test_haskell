module Tools (
	scc, prd, foldlMaybe, modifyList
) where

scc, prd :: (Ord a, Enum a, Bounded a) => a -> Maybe a
scc x	| x < maxBound = Just $ succ x
	| otherwise = Nothing
prd x	| x > minBound = Just $ pred x
	| otherwise = Nothing

foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybe = foldlMaybeBool False

foldlMaybeBool :: Bool -> (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybeBool True _ x [] = Just x
foldlMaybeBool False _ _ [] = Nothing
foldlMaybeBool j op x (y : ys) = case x `op` y of
	Just x' -> foldlMaybeBool True op x' ys
	_ -> foldlMaybeBool j op x ys

modifyList :: [a] -> Int -> (a -> a) -> [a]
modifyList xs n f = take n xs ++ [f $ xs !! n] ++ drop (n + 1) xs
