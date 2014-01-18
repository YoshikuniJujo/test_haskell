import Data.List (unfoldr)

myHead :: [a] -> a
myHead (h : _) = h
myHead [] = error "myHead: empty list"

myTail :: [a] -> [a]
myTail (_ : t) = t

myFoldl op s [] = s
myFoldl op s (x : xs) = foldl op (s `op` x) xs

myEnumFromTo, myEnumFromTo' :: (Ord a, Num a) => a -> a -> [a]
myEnumFromTo f t
	| f > t = []
	| otherwise = f : myEnumFromTo (f + 1) t

myEnumFromTo' f0 t = flip unfoldr f0 $ \f -> if f > t
	then Nothing
	else Just (f, f + 1)

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfoldr $ \x -> Just (x, f x)

factors :: Int -> [Int]
factors 1 = []
factors n = f : factors (n `div` f)
	where
	f = head $ filter ((== 0) . (n `mod`)) [2 ..]

factors' :: Int -> [Int]
factors' = unfoldr $ \n -> case n of
	1 -> Nothing
	_ -> Just (f, n `div` f)
		where
		f = head $ filter ((== 0) . (n `mod`)) [2 ..]

collatz :: Int -> [Int]
collatz = iterate $ \n -> if even n
	then n `div` 2
	else 3 * n + 1
