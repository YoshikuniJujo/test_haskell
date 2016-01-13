{-# LANGUAGE TupleSections, TypeFamilies #-}

module MapB (Map, empty, lookup, insert, delete) where

import Prelude hiding (lookup)

type Map k v = Tree k v (Tip k v)

data Tree k v a = Zero a | Succ (Tree k v (Node k v a)) deriving Show
data Node k v a = Nd2 a k v a | Nd3 a k v a k v a deriving Show
data Tip k v = Tip deriving Show

empty :: Tree k v (Tip k v)
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
	type Key a
	type Val a
	lu :: Key a -> a -> Maybe (Val a)
	ins :: Key a -> Val a -> a -> Either a (a, Key a, Val a, a)
	del :: Key a -> Node (Key a) (Val a) a ->
		Either (Node (Key a) (Val a) a) a
	mx :: Node (Key a) (Val a) a ->
		(Key a, Val a, Either (Node (Key a) (Val a) a) a)

instance Eq k => IsNode (Tip k v) where
	type Key (Tip k v) = k
	type Val (Tip k v) = v
	lu _ _ = Nothing
	ins k v _ = Right (Tip, k, v, Tip)
	del k t@(Nd2 _ d _ _)
		| k == d = Right Tip
		| otherwise = Left t
	del k t@(Nd3 _ c vc _ f vf _)
		| k == c = Left $ Nd2 Tip f vf Tip
		| k == f = Left $ Nd2 Tip c vc Tip
		| otherwise = Left t
	mx (Nd2 _ d vd _) = (d, vd, Right Tip)
	mx (Nd3 _ c vc _ f vf _) = (f, vf, Left $ Nd2 Tip c vc Tip)

instance (Ord k, IsNode a, k ~ Key a, v ~ Val a) => IsNode (Node k v a) where
	type Key (Node k v a) = k
	type Val (Node k v a) = v
	lu k (Nd2 l d vd r)
		| k < d = lu k l
		| k == d = Just vd
		| otherwise = lu k r
	lu k (Nd3 l c vc o f vf r)
		| k < c = lu k l
		| k == c = Just vc
		| k < f = lu k o
		| k == f = Just vf
		| otherwise = lu k r
	ins k v t@(Nd2 l d vd r)
		| k < d = Left $ case ins k v l of
			Left l' -> Nd2 l' d vd r
			Right (k, a, va, m) -> Nd3 k a va m d vd r
		| k == d = Left t
		| otherwise = Left $ case ins k v r of
			Left r' -> Nd2 l d vd r'
			Right (q, g, vg, s) -> Nd3 l d vd q g vg s
	ins k v t@(Nd3 l c vc o f vf r)
		| k < c = case ins k v l of
			Left l' -> Left $ Nd3 l' c vc o f vf r
			Right (k, a, va, m) ->
				Right (Nd2 k a va m, c, vc, Nd2 o f vf r)
		| k == c = Left t
		| k < f = case ins k v o of
			Left o' -> Left $ Nd3 l c vc o' f vf r
			Right (n, d, vd, p) ->
				Right (Nd2 l c vc n, d, vd, Nd2 p f vf r)
		| k == f = Left t
		| otherwise = case ins k v r of
			Left r' -> Left $ Nd3 l c vc o f vf r'
			Right (q, g, vg, s) ->
				Right (Nd2 l c vc o, f, vf, Nd2 q g vg s)
	del k (Nd2 l d vd r)
		| k <= d = case if k < d then (d, vd, del k l) else mx l of
			(d', vd', Left l') -> Left $ Nd2 l' d' vd' r
			(d', vd', Right l') -> case r of
				Nd2 q g vg s -> Right $ Nd3 l' d' vd' q g vg s
				Nd3 q g vg r' h vh s -> Left $ Nd2
					(Nd2 l' d' vd' q)
					g vg
					(Nd2 r' h vh s)
		| otherwise = case del k r of
			Left r' -> Left $ Nd2 l d vd r'
			Right r' -> case l of
				Nd2 k a va m -> Right $ Nd3 k a va m d vd r'
				Nd3 k a va l' b vb m -> Left $
					Nd2 (Nd2 k a va l') b vb (Nd2 m d vd r')
	del k (Nd3 l c vc o f vf r)
		| k <= c = Left $ case if k < c then (c, vc, del k l) else mx l of
			(c', vc', Left l') -> Nd3 l' c' vc' o f vf r
			(c', vc', Right l') -> case o of
				Nd2 n d vd p -> Nd2
					(Nd3 l' c' vc' n d vd p)
					f vf
					r
				Nd3 n d vd o' e ve p -> Nd3
					(Nd2 l' c' vc' n)
					d vd
					(Nd2 o' e ve p) f vf r
		| k <= f = Left $ case if k < f then (f, vf, del k o) else mx o of
			(f', vf', Left o') -> Nd3 l c vc o' f' vf' r
			(f', vf', Right o') -> case l of
				Nd2 k a va m -> Nd2
					(Nd3 k a va m c vc o')
					f' vf'
					r
				Nd3 k a va l' b vb m -> Nd3
					(Nd2 k a va l')
					b vb
					(Nd2 m c vc o')
					f vf
					r
		| otherwise = Left $ case del k r of
			Left r' -> Nd3 l c vc o f vf r'
			Right r' -> case o of
				Nd2 n d vd p -> Nd2
					l
					c vc
					(Nd3 n d vd p f vf r')
				Nd3 n d vd o' e ve p -> Nd3
					l
					c vc
					(Nd2 n d vd o')
					e ve
					(Nd2 p f vf r')
	mx (Nd2 l d vd r) = case mx r of
		(u, v, Left r') -> (u, v, Left $ Nd2 l d vd r')
		(u, v, Right r') -> (u , v,) $ case l of
			Nd2 k a va m -> Right $ Nd3 k a va m d vd r'
			Nd3 k a va l' b vb m -> Left $
				Nd2 (Nd2 k a va l') b vb (Nd2 m d vd r')
	mx (Nd3 l c vc o f vf r) = case mx r of
		(u, v, Left r') -> (u, v, Left $ Nd3 l c vc o f vf r')
		(u, v, Right r') -> (u, v,) . Left $ case o of
			Nd2 n d vd p -> Nd2 l c vc (Nd3 n d vd p f vf r')
			Nd3 n d vd o' e ve p ->
				Nd3 l c vc (Nd2 n d vd o') e ve (Nd2 p f vf r')

lookup :: (Ord k, IsNode a, k ~ Key a, v ~ Val a) => k -> Tree k v a -> Maybe v
lookup k (Zero n) = lu k n
lookup k (Succ t) = lookup k t

insert :: (Ord k, IsNode a, k ~ Key a, v ~ Val a) =>
	k -> v -> Tree k v a -> Tree k v a
insert k v (Zero n) = case ins k v n of
	Left n' -> Zero n'
	Right (l, d, vd, r) -> Succ . Zero $ Nd2 l d vd r
insert k v (Succ t) = Succ $ insert k v t

delete :: (Ord k, IsNode a, k ~ Key a, v ~ Val a) =>
	k -> Tree k v a -> Tree k v a
delete k (Succ (Zero n)) = case del k n of
	Left n' -> Succ $ Zero n'
	Right n' -> Zero n'
delete k (Succ t) = Succ $ delete k t
delete _ t = t
