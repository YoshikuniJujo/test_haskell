module SetBM (Set, empty, member, insert, delete) where

type Set = Tree ()

data Tree a = Zero a | Succ (Tree (Node a)) deriving Show
data Node a = Node2 a Int a | Node3 a Int a Int a deriving Show

empty :: Tree ()
empty = Zero ()

{-

  l   d   r
+-+-+   +-+-+
| a |   | g |
k   m   q   s

    l     d     r
+---+---+   +---+---+
| a | b |   | g | h |
k   l'  m   q   r'  s

  l   c   o   f   r
+-+-+   +-+-+   +-+-+
| a |   | d |   | g |
k   m   n   p   q   s

    l     c     o     f     r
+---+---+   +---+---+   +---+---+
| a | b |   | d | e |   | g | h |
k   l'  m   n   o'  p   q   r'  s

-}

class IsNode a where
	memberN :: Int -> a -> Bool
	insertN :: Int -> a -> Either a (a, Int, a)
	deleteN :: Int -> Node a -> Either (Node a) a
	popMinN :: Node a -> (Int, Either (Node a) a)
	popMaxN :: Node a -> (Int, Either (Node a) a)

instance IsNode () where
	memberN _ _ = False
	insertN v _ = Right ((), v, ())
	deleteN v t@(Node2 _ d _)
		| v == d = Right ()
		| otherwise = Left t
	deleteN v t@(Node3 _ c _ f _)
		| v == c = Left $ Node2 () f ()
		| v == f = Left $ Node2 () c ()
		| otherwise = Left t
	popMinN (Node2 _ d _) = (d, Right ())
	popMinN (Node3 _ c _ f _) = (c, Left $ Node2 () f ())
	popMaxN (Node2 _ d _) = (d, Right ())
	popMaxN (Node3 _ c _ f _) = (f, Left $ Node2 () c ())

instance IsNode a => IsNode (Node a) where
	memberN v (Node2 l d r)
		| v < d = memberN v l
		| v == d = True
		| otherwise = memberN v r
	memberN v (Node3 l c o f r)
		| v < c = memberN v l
		| v == c = True
		| v < f = memberN v o
		| v == f = True
		| otherwise = memberN v r
	insertN v t@(Node2 l d r)
		| v < d = Left $ case insertN v l of
			Left l' -> Node2 l' d r
			Right (k, a, m) -> Node3 k a m d r
		| v == d = Left t
		| otherwise = Left $ case insertN v r of
			Left r' -> Node2 l d r'
			Right (q, g, s) -> Node3 l d q g s
	insertN v t@(Node3 l c o f r)
		| v < c = case insertN v l of
			Left l' -> Left $ Node3 l' c o f r
			Right (k, a, m) -> Right (Node2 k a m, c, Node2 o f r)
		| v == c = Left t
		| v < f = case insertN v o of
			Left o' -> Left $ Node3 l c o' f r
			Right (n, d, p) -> Right (Node2 l c n, d, Node2 p f r)
		| v == f = Left t
		| otherwise = case insertN v r of
			Left r' -> Left $ Node3 l c o f r'
			Right (q, g, s) -> Right (Node2 l c o, f, Node2 q g s)
	deleteN v (Node2 l d r)
		| v < d = case deleteN v l of
			Left l' -> Left $ Node2 l' d r
			Right l' -> case r of
				Node2 q g s -> Right $ Node3 l' d q g s
				Node3 q g r' h s -> Left $
					Node2 (Node2 l' d q) g (Node2 r' h s)
		| v == d = case popMaxN l of
			(u, Left l') -> Left $ Node2 l' u r
			(u, Right l') -> case r of
				Node2 q g s -> Right $ Node3 l' u q g s
				Node3 q g r' h s -> Left $
					Node2 (Node2 l' u q) g (Node2 r' h s)
		| otherwise = case deleteN v r of
			Left r' -> Left $ Node2 l d r'
			Right r' -> case l of
				Node2 k a m -> Right $ Node3 k a m d r'
				Node3 k a l' b m -> Left $
					Node2 (Node2 k a l') b (Node2 m d r')
	deleteN v (Node3 l c o f r)
		| v < c = case deleteN v l of
			Left l' -> Left $ Node3 l' c o f r
			Right l' -> case o of
				Node2 n d p -> Left $
					Node2 (Node3 l' c n d p) f r
				Node3 n d o' e p -> Left $
					Node3 (Node2 l' c n) d (Node2 o' e p) f r
		| v == c = case popMaxN l of
			(u, Left l') -> Left $ Node3 l' u o f r
			(u, Right l') -> case o of
				Node2 n d p -> Left $ Node2 (Node3 l' u n d p) f r
				Node3 n d o' e p -> Left $
					Node3 (Node2 l' u n) d (Node2 o' e p) f r
		| v < f = case deleteN v o of
			Left m' -> Left $ Node3 l c m' f r
			Right m' -> Left $ rotateM l c m' f r
		| v == f = case popMaxN o of
			(u, Left m') -> Left $ Node3 l c m' u r
			(u, Right m') -> Left $ rotateM l c m' u r
		| otherwise = case deleteN v r of
			Left r' -> Left $ Node3 l c o f r'
			Right r' -> case o of
				Node2 n d p -> Left $
					Node2 l c (Node3 n d p f r')
				Node3 n d o' e p -> Left $
					Node3 l c (Node2 n d o') e (Node2 p f r')
	popMinN (Node2 l d r) = case popMinN l of
		(u, Left l') -> (u, Left $ Node2 l' d r)
		(u, Right l') -> case r of
			Node2 q g s -> (u, Right $ Node3 l' d q g s)
			Node3 q g r' h s -> (u, Left $
				Node2 (Node2 l' d q) g (Node2 r' h s))
	popMinN (Node3 l c o f r) = case popMinN l of
		(u, Left l') -> (u, Left $ Node3 l' c o f r)
		(u, Right l') -> case o of
			Node2 n d p -> (u, Left $ Node2 (Node3 l' c n d p) f r)
			Node3 n d o' e p -> (u, Left $
				Node3 (Node2 l' c n) d (Node2 o' e p) f r)
	popMaxN (Node2 l d r) = case popMaxN r of
		(u, Left r') -> (u, Left $ Node2 l d r')
		(u, Right r') -> case l of
			Node2 k a m -> (u, Right $ Node3 k a m d r')
			Node3 k a l' b m -> (u, Left $
				Node2 (Node2 k a l') b (Node2 m d r'))
	popMaxN (Node3 l c o f r) = case popMaxN r of
		(u, Left r') -> (u, Left $ Node3 l c o f r')
		(u, Right r') -> case o of
			Node2 n d p -> (u, Left $ Node2 l c (Node3 n d p f r'))
			Node3 n d o' e p -> (u, Left $
				Node3 l c (Node2 n d o') e (Node2 p f r'))

rotateM :: (Node a) -> Int -> a -> Int -> (Node a) -> Node (Node a)
rotateM (Node3 k a l' b m) c o f r = Node3 (Node2 k a l') b (Node2 m c o) f r
rotateM l c o f (Node3 q g r' h s) = Node3 l c (Node2 o f q) g (Node2 r' h s)
rotateM (Node2 k a m) c o f r = Node2 (Node3 k a m c o) f r

member :: IsNode a => Int -> Tree a -> Bool
member v (Zero n) = memberN v n
member v (Succ t) = member v t

insert :: IsNode a => Int -> Tree a -> Tree a
insert v (Zero n) = case insertN v n of
	Left n' -> Zero n'
	Right (l, d, r) -> Succ . Zero $ Node2 l d r
insert v (Succ t) = Succ $ insert v t

delete :: IsNode a => Int -> Tree a -> Tree a
delete v (Succ (Zero n)) = case deleteN v n of
	Left n' -> Succ $ Zero n'
	Right n' -> Zero n'
delete v (Succ t) = Succ $ delete v t
delete _ t = t
