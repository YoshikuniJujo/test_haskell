module SetBM (Set, empty, member, insert) where

-- import Data.Maybe

data Set a = Zero a | Succ (Set (Node a)) deriving Show
data Node a = Node2 a Int a | Node3 a Int a Int a deriving Show

empty :: Set ()
empty = Zero ()

class IsNode a where
	memberN :: Int -> a -> Bool
	insertN :: Int -> a -> Either a (a, Int, a)
--	deleteN :: Int -> a -> Either a 
--	popMinN :: Node a -> Maybe (Int, Either (Node a) a)
--	maxN :: a -> Maybe Int
	deleteMinN :: Node a -> Either (Node a) a

instance IsNode () where
	memberN _ _ = False
	insertN a _ = Right ((), a, ())
--	popMinN _ = Nothing
	deleteMinN (Node2 _ _ _) = Right ()
	deleteMinN (Node3 _ _ _ b _) = Left $ Node2 () b ()
--	deleteN _ _ = ()
--	minN _ = Nothing
--	maxN _ = Nothing

instance IsNode a => IsNode (Node a) where
	memberN a (Node2 l b r)
		| a < b = memberN a l
		| a == b = True
		| otherwise = memberN a r
	memberN a (Node3 l b m c r)
		| a < b = memberN a l
		| a == b = True
		| a < c = memberN a m
		| a == c = True
		| otherwise = memberN a r
	insertN a t@(Node2 l b r)
		| a < b = case insertN a l of
			Left l' -> Left $ Node2 l' b r
			Right (l', c, m) -> Left $ Node3 l' c m b r
		| a == b = Left t
		| otherwise = case insertN a r of
			Left r' -> Left $ Node2 l b r'
			Right (m, c, r') -> Left $ Node3 l b m c r'
	insertN a t@(Node3 l b m c r)
		| a < b = case insertN a l of
			Left l' -> Left $ Node3 l' b m c r
			Right (l', d, m') ->
				Right (Node2 l' d m', b, Node2 m c r)
		| a == b = Left t
		| a < c = case insertN a m of
			Left m' -> Left $ Node3 l b m' c r
			Right (m', d, m'') ->
				Right (Node2 l b m', d, Node2 m'' c r)
		| a == c = Left t
		| otherwise = case insertN a r of
			Left r' -> Left $ Node3 l b m c r'
			Right (m', d, r') ->
				Right (Node2 l b m, c, Node2 m' d r')
	deleteMinN (Node2 l a r) = case deleteMinN l of
		Left l' -> Left $ Node2 l' a r
		Right l' -> case r of
			Node2 m b r' -> Right $ Node3 l' a m b r'
			Node3 m b m' c r' -> Left $
				Node2 (Node2 l' a m) b (Node2 m' c r')
	deleteMinN (Node3 l a m b r) = case deleteMinN l of
		Left l' -> Left $ Node3 l' a m b r
		Right l' -> case m of
			Node2 m' c m'' -> Left $
				Node2 (Node3 l' a m' c m'') b r
			Node3 m' c m'' d m''' -> Left $
				Node3 (Node2 l' a m') c (Node2 m'' d m''') b r
--	popMin (Node2 l a r)
--	deleteN a t@(Node2 l b r)
--		| a < b = 
--	minN (Node2 l a _) = Just . fromMaybe a $ minN l
--	minN (Node3 l a _ _ _) = Just . fromMaybe a $ minN l
--	maxN (Node2 _ a r) = Just . fromMaybe a $ maxN r
--	maxN (Node3 _ _ _ b r) = Just . fromMaybe b $ maxN r

member :: IsNode a => Int -> Set a -> Bool
member a (Zero n) = memberN a n
member a (Succ t) = member a t

insert :: IsNode a => Int -> Set a -> Set a
insert a (Zero n) = case insertN a n of
	Left n' -> Zero n'
	Right (l, b, r) -> Succ . Zero $ Node2 l b r
insert a (Succ t) = Succ $ insert a t

deleteMin :: IsNode a => Set a -> Set a
deleteMin (Succ (Zero n)) = case deleteMinN n of
	Left n' -> Succ $ Zero n'
	Right n' -> Zero n'
deleteMin (Succ t) = Succ $ deleteMin t
deleteMin t = t

-- minT, maxT :: IsNode a => Set a -> Maybe Int
-- minT (Zero n) = minN n
-- minT (Succ t) = minT t

-- maxT (Zero n) = maxN n
-- maxT (Succ t) = maxT t

{-
checkBalance :: (Node (Node (Node ()))) -> Either (Node (Node (Node ()))) (Node (Node ()))
checkBalance (Node2 l a r)
-}
