data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

sample :: BinTree Int
sample = Node (Node (Leaf 10) (Leaf 25)) (Node (Leaf 100) (Leaf 55))

instance Foldable BinTree where
	foldr op v (Leaf x) = x `op` v
	foldr op v (Node t1 t2) = foldr op  (foldr op v t2) t1
	foldMap f (Leaf x) = f x
	foldMap f (Node t1 t2) = foldMap f t1 `mappend` foldMap f t2

-- foldr f z t = foldMap f t z
-- ffoldr op (Leaf x) = (x `op`)
-- ffoldr op (Node t1 t2) = ffoldr op t1 . ffoldr op t2
-- ffoldr op (Leaf x) v = x `op` v
-- ffoldr op (Node t1 t2) v = ffoldr op t1 (ffoldr op t2 v)
-- foldr op v (Leaf x) = x `op` v
-- foldr op v (Node t1 t2) v = foldr op (foldr op v t2) t1
