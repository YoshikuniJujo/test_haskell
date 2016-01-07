module SetBM (Set, empty, member, insert) where

import Control.Applicative ((<$>))
import Control.Arrow
-- import Data.Maybe

type Set = Tree ()

data Tree a = Zero a | Succ (Tree (Node a)) deriving Show
data Node a = Node2 a Int a | Node3 a Int a Int a deriving Show

empty :: Tree ()
empty = Zero ()

class IsNode a where
	memberN :: Int -> a -> Bool
	insertN :: Int -> a -> Either a (a, Int, a)
	popMinN :: Node a -> (Int, Either (Node a) a)
	popMaxN :: Node a -> (Int, Either (Node a) a)

instance IsNode () where
	memberN _ _ = False
	insertN a _ = Right ((), a, ())
	popMinN (Node2 _ a _) = (a, Right ())
	popMinN (Node3 _ a _ b _) = (a, Left $ Node2 () b ())
	popMaxN (Node2 _ a _) = (a, Right ())
	popMaxN (Node3 _ a _ b _) = (b, Left $ Node2 () a ())

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
	popMinN (Node2 l a r) = case popMinN l of
		(n, Left l') -> (n, Left $ Node2 l' a r)
		(n, Right l') -> case r of
			Node2 m b r' -> (n, Right $ Node3 l' a m b r')
			Node3 m b m' c r' -> (n, Left $
				Node2 (Node2 l' a m) b (Node2 m' c r'))
	popMinN (Node3 l a m b r) = case popMinN l of
		(n, Left l') -> (n, Left $ Node3 l' a m b r)
		(n, Right l') -> case m of
			Node2 m' c m'' -> (n, Left $
				Node2 (Node3 l' a m' c m'') b r)
			Node3 m' c m'' d m''' -> (n, Left $
				Node3 (Node2 l' a m') c (Node2 m'' d m''') b r)
	popMaxN (Node2 l a r) = case popMaxN r of
		(x, Left r') -> (x, Left $ Node2 l a r')
		(x, Right r') -> case l of
			Node2 l' b m -> (x, Right $ Node3 l' b m a r')
			Node3 l' b m c m' -> (x, Left $
				Node2 (Node2 l' b m) c (Node2 m' a r'))
	popMaxN (Node3 l a m b r) = case popMaxN r of
		(x, Left r') -> (x, Left $ Node3 l a m b r')
		(x, Right r') -> case m of
			Node2 m' c m'' -> (x, Left $
				Node2 l a (Node3 m' c m'' b r'))
			Node3 m' c m'' d m''' -> (x, Left $
				Node3 l a (Node2 m' c m'') d (Node2 m''' b r'))

member :: IsNode a => Int -> Tree a -> Bool
member a (Zero n) = memberN a n
member a (Succ t) = member a t

insert :: IsNode a => Int -> Tree a -> Tree a
insert a (Zero n) = case insertN a n of
	Left n' -> Zero n'
	Right (l, b, r) -> Succ . Zero $ Node2 l b r
insert a (Succ t) = Succ $ insert a t

popMin :: IsNode a => Tree a -> Maybe (Int, Tree a)
popMin (Succ (Zero n)) = Just $ case popMinN n of
	(mn, Left n') -> (mn, Succ $ Zero n')
	(mn, Right n') -> (mn, Zero n')
popMin (Succ t) = (Succ `second`) <$> popMin t
popMin _ = Nothing

popMax :: IsNode a => Tree a -> Maybe (Int, Tree a)
popMax (Succ (Zero n)) = Just $ case popMaxN n of
	(mx, Left n') -> (mx, Succ $ Zero n')
	(mx, Right n') -> (mx, Zero n')
popMax (Succ t) = (Succ `second`) <$> popMax t
popMax _ = Nothing
