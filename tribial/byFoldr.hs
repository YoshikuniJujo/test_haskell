import Prelude hiding (
	map, (++), filter, head, last, tail, init, null, length, (!!), reverse,
	foldl, foldl1, foldr1,
	and, or, any, all, sum, product, concat, concatMap, maximum, minimum,
	scanl, scanl1, scanr, scanr1,
	repeat, replicate, cycle,
	take, drop, splitAt, takeWhile, dropWhile, span, break,
	elem, notElem, lookup,
	zip, zip3, zipWith, zipWith3, unzip, unzip3,
	lines, words, unlines, unwords)

map f = foldr ((:) . f) []
(++) = flip (foldr (:))
filter p = foldr (\x -> if p x then (x :) else id) []
head (x : _) = x
tail (_ : xs) = xs
init (x : xs) = foldr f (const []) xs x where f x g = (: g x)
--	where f = (flip (:) .) . flip ($)
last = foldr1 $ const id
null [] = True
null _ = False
length = foldr (const (+ 1)) 0
(!!) _ n | n < 0 = error "out of index"
(!!) xs n = foldr f (error "out of index") xs n
	where f x _ 0 = x; f _ g n = g $ n - 1
reverse = foldl (flip (:)) []

foldl f i xs = foldr s id xs i where s x g a = g $ f a x
foldl1 f (x : xs) = foldl f x xs
foldr1 f (x : xs) = foldr s id xs x where s x g = (`f` g x)

and = foldr (&&) False
or = foldr (||) False
any p = foldr ((||) . p) False
all p = foldr ((&&) . p) True
sum = foldr (+) 0
product = foldr (*) 1
concat = foldr (++) []
concatMap f = foldr ((++) . f) []
-- maximum = foldr1 max
-- minimum = foldr1 min

scanl f x xs = foldr s (: []) xs x where s x g a = a : g (f a x)
scanl1 f (x : xs) = scanl f x xs
scanr f = foldr s . (: []) where s x va@(v : _) = f x v : va
scanr1 f (x : xs) = foldr s (: []) xs x
	where s x g a = let va@(v : _) = g x in f a v : va

scanr' f q0 (x : xs) = f x q : qs where qs@(q : _) = scanr' f q0 xs
scanr' _ q0 [] = [q0]

scanr1' _ [] = []
scanr1' _ [x] = [x]
scanr1' f (x : xs) = f x q : qs where qs@(q : _) = scanr1' f xs

repeat = iterate id
replicate n = take n . repeat
cycle = concat . repeat

take = flip $ foldr f (const []) where f _ _ 0 = []; f x g n = x : g (n - 1)
drop = flip $ foldr f (const []) where f x g 0 = x : g 0; f _ g n = g $ n - 1
splitAt n xs = (take n xs, drop n xs)

takeWhile p = foldr f [] where f x xs | p x = x : xs | otherwise = []
dropWhile p xs = foldr f (const []) xs True where
	f x g True
		| p x = g True
		| otherwise = x : g False
	f x g False = x : g False

zipWith f = foldr g (const [])
	where
	g x h [] = []
	g x h (y : ys) = f x y : h ys
