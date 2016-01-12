{-# LANGUAGE TupleSections, TypeFamilies #-}

module SetB (Set, empty, member, insert, delete) where

type Set v = Tree v ()

data Tree v a = Zero a | Succ (Tree v (Node v a)) deriving Show
data Node v a = Nd2 a v a | Nd3 a v a v a deriving Show
data Tip v = Tip deriving Show

empty :: Tree v (Tip v)
empty = Zero Tip

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
	type Elem a
	mem :: Elem a -> a -> Bool
	ins :: Elem a -> a -> Either a (a, Elem a, a)
	del :: Elem a -> Node (Elem a) a -> Either (Node (Elem a) a) a
	mx :: Node (Elem a) a -> (Elem a, Either (Node (Elem a) a) a)

instance Eq v => IsNode (Tip v) where
	type Elem (Tip v) = v
	mem _ _ = False
	ins v _ = Right (Tip, v, Tip)
	del v t@(Nd2 _ d _)
		| v == d = Right Tip
		| otherwise = Left t
	del v t@(Nd3 _ c _ f _)
		| v == c = Left $ Nd2 Tip f Tip
		| v == f = Left $ Nd2 Tip c Tip
		| otherwise = Left t
	mx (Nd2 _ d _) = (d, Right Tip)
	mx (Nd3 _ c _ f _) = (f, Left $ Nd2 Tip c Tip)

instance (Ord v, IsNode a, v ~ Elem a) => IsNode (Node v a) where
	type Elem (Node v a) = v
	mem v (Nd2 l d r)
		| v < d = mem v l
		| v == d = True
		| otherwise = mem v r
	mem v (Nd3 l c o f r)
		| v < c = mem v l
		| v == c = True
		| v < f = mem v o
		| v == f = True
		| otherwise = mem v r
	ins v t@(Nd2 l d r)
		| v < d = Left $ case ins v l of
			Left l' -> Nd2 l' d r
			Right (k, a, m) -> Nd3 k a m d r
		| v == d = Left t
		| otherwise = Left $ case ins v r of
			Left r' -> Nd2 l d r'
			Right (q, g, s) -> Nd3 l d q g s
	ins v t@(Nd3 l c o f r)
		| v < c = case ins v l of
			Left l' -> Left $ Nd3 l' c o f r
			Right (k, a, m) -> Right (Nd2 k a m, c, Nd2 o f r)
		| v == c = Left t
		| v < f = case ins v o of
			Left o' -> Left $ Nd3 l c o' f r
			Right (n, d, p) -> Right (Nd2 l c n, d, Nd2 p f r)
		| v == f = Left t
		| otherwise = case ins v r of
			Left r' -> Left $ Nd3 l c o f r'
			Right (q, g, s) -> Right (Nd2 l c o, f, Nd2 q g s)
	del v (Nd2 l d r)
		| v <= d = case if v < d then (d, del v l) else mx l of
			(d', Left l') -> Left $ Nd2 l' d' r
			(d', Right l') -> case r of
				Nd2 q g s -> Right $ Nd3 l' d' q g s
				Nd3 q g r' h s -> Left $
					Nd2 (Nd2 l' d' q) g (Nd2 r' h s)
		| otherwise = case del v r of
			Left r' -> Left $ Nd2 l d r'
			Right r' -> case l of
				Nd2 k a m -> Right $ Nd3 k a m d r'
				Nd3 k a l' b m -> Left $
					Nd2 (Nd2 k a l') b (Nd2 m d r')
	del v (Nd3 l c o f r)
		| v <= c = Left $ case if v < c then (c, del v l) else mx l of
			(c', Left l') -> Nd3 l' c' o f r
			(c', Right l') -> case o of
				Nd2 n d p -> Nd2 (Nd3 l' c' n d p) f r
				Nd3 n d o' e p ->
					Nd3 (Nd2 l' c' n) d (Nd2 o' e p) f r
		| v <= f = Left $ case if v < f then (f, del v o) else mx o of
			(f', Left o') -> Nd3 l c o' f' r
			(f', Right o') -> case l of
				Nd2 k a m -> Nd2 (Nd3 k a m c o') f' r
				Nd3 k a l' b m ->
					Nd3 (Nd2 k a l') b (Nd2 m c o') f r
		| otherwise = Left $ case del v r of
			Left r' -> Nd3 l c o f r'
			Right r' -> case o of
				Nd2 n d p -> Nd2 l c (Nd3 n d p f r')
				Nd3 n d o' e p ->
					Nd3 l c (Nd2 n d o') e (Nd2 p f r')
	mx (Nd2 l d r) = case mx r of
		(u, Left r') -> (u, Left $ Nd2 l d r')
		(u, Right r') -> (u ,) $ case l of
			Nd2 k a m -> Right $ Nd3 k a m d r'
			Nd3 k a l' b m -> Left $ Nd2 (Nd2 k a l') b (Nd2 m d r')
	mx (Nd3 l c o f r) = case mx r of
		(u, Left r') -> (u, Left $ Nd3 l c o f r')
		(u, Right r') -> (u ,) . Left $ case o of
			Nd2 n d p -> Nd2 l c (Nd3 n d p f r')
			Nd3 n d o' e p -> Nd3 l c (Nd2 n d o') e (Nd2 p f r')

member :: (Ord v, IsNode a, v ~ Elem a) => v -> Tree v a -> Bool
member v (Zero n) = mem v n
member v (Succ t) = member v t

insert :: (Ord v, IsNode a, v ~ Elem a) => v -> Tree v a -> Tree v a
insert v (Zero n) = case ins v n of
	Left n' -> Zero n'
	Right (l, d, r) -> Succ . Zero $ Nd2 l d r
insert v (Succ t) = Succ $ insert v t

delete :: (Ord v, IsNode a, v ~ Elem a) => v -> Tree v a -> Tree v a
delete v (Succ (Zero n)) = case del v n of
	Left n' -> Succ $ Zero n'
	Right n' -> Zero n'
delete v (Succ t) = Succ $ delete v t
delete _ t = t
