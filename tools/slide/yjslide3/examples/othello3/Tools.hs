module Tools (scc, prd, foldMaybe, modifyList, flipE) where

scc, prd :: (Ord a, Enum a, Bounded a) => a -> Maybe a
scc x	| x < maxBound = Just $ succ x
	| otherwise = Nothing
prd x	| x > minBound = Just $ pred x
	| otherwise = Nothing

foldMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldMaybe = foldMaybeBool False

foldMaybeBool :: Bool -> (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldMaybeBool True _ x [] = Just x
foldMaybeBool False _ _ [] = Nothing
foldMaybeBool j op x (y : ys) = case x `op` y of
	Just x' -> foldMaybeBool True op x' ys
	_ -> foldMaybeBool j op x ys

modifyList :: [a] -> Int -> (a -> a) -> [a]
modifyList xs n f = take n xs ++ [f $ xs !! n] ++ drop (n + 1) xs

flipE :: (Enum a, Bounded a) => a -> a
flipE x = toEnum $ fromEnum (maxBound `asTypeOf` x) - fromEnum x
