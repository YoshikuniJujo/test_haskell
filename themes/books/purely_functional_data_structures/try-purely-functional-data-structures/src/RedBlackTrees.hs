{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RedBlackTrees where

data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b)
	| x < y = member x a
	| x > y = member x b
	| otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a' y' b'
	where
	ins E = T R E x E
	ins s'@(T c a y b)
--		| x < y = balance c (ins a) y b
--		| x > y = balance c a y (ins b)
		| x < y = lbalance c (ins a) y b
		| x > y = rbalance c a y (ins b)
		| otherwise = s'
	T _ a' y' b' = ins s

insert' :: Ord a => a -> Tree a -> Tree a
insert' x s = T B a' y' b'
	where
	ins E = T R E x E
	ins (T c E y b) | x < y = T c (T R E x b) y b
	ins (T c a y E) | x > y = T c a y (T R E x E)
	ins s'@(T c a y b)
		| x < y = case a of
			E -> T c (T R E x b) y b
			T _ _ z _
				| x < z -> llbalance c (ins a) y b
				| x > z -> lrbalance c (ins a) y b
				| otherwise -> T c (ins a) y b
		| x > y = case b of
			E -> T c a y (T R E x E)
			T _ _ z _
				| x < z -> rlbalance c a y (ins b)
				| x > z -> rrbalance c a y (ins b)
				| otherwise -> T c a y (ins b)
		| otherwise = s'
	T _ a' y' b' = ins s

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B t1 x0 t2 = case (t1, x0, t2) of
	(T R (T R a x b) y c, z, d) -> f a x b y c z d
	(T R a x (T R b y c), z, d) -> f a x b y c z d
	(a, x, T R (T R b y c) z d) -> f a x b y c z d
	(a, x, T R b y (T R c z d)) -> f a x b y c z d
	_ -> T B t1 x0 t2
	where f a x b y c z d = T R (T B a x b) y (T B c z d)
balance R t1 x0 t2 = T R t1 x0 t2

lbalance, rbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lbalance B t1 x0 t2 = case (t1, x0, t2) of
	(T R (T R a x b) y c, z, d) -> f a x b y c z d
	(T R a x (T R b y c), z, d) -> f a x b y c z d
	_ -> T B t1 x0 t2
	where f a x b y c z d = T R (T B a x b) y (T B c z d)
lbalance R t1 x0 t2 = T R t1 x0 t2

rbalance B t1 x0 t2 = case (t1, x0, t2) of
	(a, x, T R (T R b y c) z d) -> f a x b y c z d
	(a, x, T R b y (T R c z d)) -> f a x b y c z d
	_ -> T B t1 x0 t2
	where f a x b y c z d = T R (T B a x b) y (T B c z d)
rbalance R t1 x0 t2 = T R t1 x0 t2

llbalance, lrbalance, rlbalance, rrbalance ::
	Color -> Tree a -> a -> Tree a -> Tree a
llbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
llbalance c t1 x0 t2 = T c t1 x0 t2

lrbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lrbalance c t1 x0 t2 = T c t1 x0 t2

rlbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rlbalance c t1 x0 t2 = T c t1 x0 t2

rrbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rrbalance c t1 x0 t2 = T c t1 x0 t2

{-
mergeOrdTrees :: Tree a -> Tree a -> Tree a
mergeOrdTrees E t = t
mergeOrdTrees t E = t
mergeOrdTrees (T _ a x b) (T _ c y d) = balance B a x (T R c y d)
-}

-- fromOrdList :: [a] -> Tree a
-- fromOrdList = 

-- fromOrdListGen :: Tree a -> [(a, Tree a)] -> (Tree a, [(a, Tree a)])
-- fromOrdListGen a ((x, b) : ts) = (T B a x b, 
--	where (t, ts') = fromOrdListGen ts

fromOrdList :: [a] -> Tree a
fromOrdList xs = case toTrees xs of
	Nothing -> E
	Just (t, ts) -> fromOrdListGen t ts

toTrees :: [a] -> Maybe (Tree a, [(a, Tree a)])
toTrees [] = Nothing
toTrees [x] = Just (T B E x E, [])
toTrees [x, y] = Just (T B E x (T R E y E), [])
toTrees (x : y : xs) = case toTrees xs of
	Just (t, ts) -> Just (T B E x E, (y, t) : ts)
	Nothing -> error "never occur"

fromOrdListGen :: Tree a -> [(a, Tree a)] -> Tree a
fromOrdListGen t ts = case fromOrdListGen1Level t ts of
	Nothing -> t
	Just (t', ts') -> fromOrdListGen t' ts'

fromOrdListGen1Level :: Tree a -> [(a, Tree a)] -> Maybe (Tree a, [(a, Tree a)])
fromOrdListGen1Level _ [] = Nothing
fromOrdListGen1Level a [(x, b)] = return (T B a x b, [])
fromOrdListGen1Level a [(x, b), (y, c)] = return (T B a x (T R b y c), [])
fromOrdListGen1Level a ((x, b) : (y, c) : ts) = do
	(t, ts') <- fromOrdListGen1Level c ts
	return (T B a x b, (y, t) : ts')

countBlacks :: Tree a -> [Int]
countBlacks E = [1]
countBlacks (T R a _ b) = countBlacks a ++ countBlacks b
countBlacks (T B a _ b) = (1 +) <$> countBlacks a ++ countBlacks b

checkRedRed :: Tree a -> Bool
checkRedRed E = False
checkRedRed (T R (T R _ _ _) _ _) = True
checkRedRed (T R _ _ (T R _ _ _)) = True
checkRedRed (T _ a _ b) = checkRedRed a || checkRedRed b
