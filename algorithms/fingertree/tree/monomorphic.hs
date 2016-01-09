{-# LANGUAGE TupleSections #-}

module SetBM (Set, empty, member, insert, delete) where

type Set = Tree ()

data Tree a = Zero a | Succ (Tree (Node a)) deriving Show
data Node a = Nd2 a Int a | Nd3 a Int a Int a deriving Show

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
	popMaxN :: Node a -> (Int, Either (Node a) a)

instance IsNode () where
	memberN _ _ = False
	insertN v _ = Right ((), v, ())
	deleteN v t@(Nd2 _ d _)
		| v == d = Right ()
		| otherwise = Left t
	deleteN v t@(Nd3 _ c _ f _)
		| v == c = Left $ Nd2 () f ()
		| v == f = Left $ Nd2 () c ()
		| otherwise = Left t
	popMaxN (Nd2 _ d _) = (d, Right ())
	popMaxN (Nd3 _ c _ f _) = (f, Left $ Nd2 () c ())

instance IsNode a => IsNode (Node a) where
	memberN v (Nd2 l d r)
		| v < d = memberN v l
		| v == d = True
		| otherwise = memberN v r
	memberN v (Nd3 l c o f r)
		| v < c = memberN v l
		| v == c = True
		| v < f = memberN v o
		| v == f = True
		| otherwise = memberN v r
	insertN v t@(Nd2 l d r)
		| v < d = Left $ case insertN v l of
			Left l' -> Nd2 l' d r
			Right (k, a, m) -> Nd3 k a m d r
		| v == d = Left t
		| otherwise = Left $ case insertN v r of
			Left r' -> Nd2 l d r'
			Right (q, g, s) -> Nd3 l d q g s
	insertN v t@(Nd3 l c o f r)
		| v < c = case insertN v l of
			Left l' -> Left $ Nd3 l' c o f r
			Right (k, a, m) -> Right (Nd2 k a m, c, Nd2 o f r)
		| v == c = Left t
		| v < f = case insertN v o of
			Left o' -> Left $ Nd3 l c o' f r
			Right (n, d, p) -> Right (Nd2 l c n, d, Nd2 p f r)
		| v == f = Left t
		| otherwise = case insertN v r of
			Left r' -> Left $ Nd3 l c o f r'
			Right (q, g, s) -> Right (Nd2 l c o, f, Nd2 q g s)
	deleteN v (Nd2 l d r)
		| v < d = case deleteN v l of
			Left l' -> Left $ Nd2 l' d r
			Right l' -> case r of
				Nd2 q g s -> Right $ Nd3 l' d q g s
				Nd3 q g r' h s -> Left $
					Nd2 (Nd2 l' d q) g (Nd2 r' h s)
		| v == d = case popMaxN l of
			(u, Left l') -> Left $ Nd2 l' u r
			(u, Right l') -> case r of
				Nd2 q g s -> Right $ Nd3 l' u q g s
				Nd3 q g r' h s -> Left $
					Nd2 (Nd2 l' u q) g (Nd2 r' h s)
		| otherwise = case deleteN v r of
			Left r' -> Left $ Nd2 l d r'
			Right r' -> case l of
				Nd2 k a m -> Right $ Nd3 k a m d r'
				Nd3 k a l' b m -> Left $
					Nd2 (Nd2 k a l') b (Nd2 m d r')
	deleteN v (Nd3 l c o f r)
		| v < c = Left $ case deleteN v l of
			Left l' -> Nd3 l' c o f r
			Right l' -> fillL l' c o f r
		| v == c = Left $ case popMaxN l of
			(u, Left l') -> Nd3 l' u o f r
			(u, Right l') -> fillL l' u o f r
		| v < f = Left $ case deleteN v o of
			Left m' -> Nd3 l c m' f r
			Right m' -> fillM l c m' f r
		| v == f = Left $ case popMaxN o of
			(u, Left m') -> Nd3 l c m' u r
			(u, Right m') -> fillM l c m' u r
		| otherwise = Left $ case deleteN v r of
			Left r' -> Nd3 l c o f r'
			Right r' -> fillR l c o f r'
	popMaxN (Nd2 l d r) = case popMaxN r of
		(u, Left r') -> (u, Left $ Nd2 l d r')
		(u, Right r') -> (u ,) $ case l of
			Nd2 k a m -> Right $ Nd3 k a m d r'
			Nd3 k a l' b m -> Left $ Nd2 (Nd2 k a l') b (Nd2 m d r')
	popMaxN (Nd3 l c o f r) = case popMaxN r of
		(u, Left r') -> (u, Left $ Nd3 l c o f r')
		(u, Right r') -> (u ,) . Left $ case o of
			Nd2 n d p -> Nd2 l c (Nd3 n d p f r')
			Nd3 n d o' e p -> Nd3 l c (Nd2 n d o') e (Nd2 p f r')

fillL :: a -> Int -> (Node a) -> Int -> (Node a) -> Node (Node a)
fillL l c (Nd3 n d o' e p) f r = Nd3 (Nd2 l c n) d (Nd2 o' e p) f r
fillL l c (Nd2 n d p) f r = Nd2 (Nd3 l c n d p) f r

fillM :: (Node a) -> Int -> a -> Int -> (Node a) -> Node (Node a)
fillM (Nd3 k a l' b m) c o f r = Nd3 (Nd2 k a l') b (Nd2 m c o) f r
fillM l c o f (Nd3 q g r' h s) = Nd3 l c (Nd2 o f q) g (Nd2 r' h s)
fillM (Nd2 k a m) c o f r = Nd2 (Nd3 k a m c o) f r

fillR :: (Node a) -> Int -> (Node a) -> Int -> a -> Node (Node a)
fillR l c (Nd3 n d o' e p) f r = Nd3 l c (Nd2 n d o') e (Nd2 p f r)
fillR l c (Nd2 n d p) f r = Nd2 l c (Nd3 n d p f r)

member :: IsNode a => Int -> Tree a -> Bool
member v (Zero n) = memberN v n
member v (Succ t) = member v t

insert :: IsNode a => Int -> Tree a -> Tree a
insert v (Zero n) = case insertN v n of
	Left n' -> Zero n'
	Right (l, d, r) -> Succ . Zero $ Nd2 l d r
insert v (Succ t) = Succ $ insert v t

delete :: IsNode a => Int -> Tree a -> Tree a
delete v (Succ (Zero n)) = case deleteN v n of
	Left n' -> Succ $ Zero n'
	Right n' -> Zero n'
delete v (Succ t) = Succ $ delete v t
delete _ t = t
