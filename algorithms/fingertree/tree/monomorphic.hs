module SetBM (Set, empty, member, insert) where

data Set a = Zero a | Succ (Set (Node a)) deriving Show
data Node a = Node2 a Int a | Node3 a Int a Int a deriving Show

empty :: Set ()
empty = Zero ()

class IsNode a where
	memberN :: Int -> a -> Bool
	insertN :: Int -> a -> Either a (a, Int, a)

instance IsNode () where
	memberN _ _ = False
	insertN a _ = Right ((), a, ())

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

member :: IsNode a => Int -> Set a -> Bool
member a (Zero n) = memberN a n
member a (Succ t) = member a t

insert :: IsNode a => Int -> Set a -> Set a
insert a (Zero n) = case insertN a n of
	Left n' -> Zero n'
	Right (l, b, r) -> Succ . Zero $ Node2 l b r
insert a (Succ t) = Succ $ insert a t
