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

fun1 :: [Int] -> [Int] -> [Int]
fun1 = foldr $ \_ na@(n : ns) -> case n of
	0 -> na
	_ -> n - 1 : na

myTake :: Int -> [a] -> [a]
myTake = curry $ unfoldr $ \s -> case s of
	(0, _) -> Nothing
	(_, []) -> Nothing
	(n, x : xs) -> Just (x, (n - 1, xs))

myTake' :: Int -> [a] -> [a]
myTake' 0 _ = []
myTake' _ [] = []
myTake' n (x : xs) = x : myTake' (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (x : xs) = myDrop (n - 1) xs

takeUntil, takeUntil' :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x : xs)
	| p x = [x]
	| otherwise = x : takeUntil p xs

takeUntil' p = foldr (\x -> if p x then const [x] else (x:)) []

takeTo p = flip foldr [] $ \x lst -> x : (if p x then [] else lst)

collatzInf n = n : collatzInf (if even n
	then n `div` 2
	else n * 3 + 1)

popFactor :: Integer -> Maybe (Integer, Integer)
popFactor 1 = Nothing
popFactor n = Just (f, n `div` f)
	where
	f = head $ filter ((== 0) . (n `mod`)) [2 .. n]

factorization :: Integer -> [Integer]
factorization = unfoldr popFactor
