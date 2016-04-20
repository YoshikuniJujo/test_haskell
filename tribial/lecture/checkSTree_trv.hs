{-# LANGUAGE TupleSections, MonadComprehensions #-}

import Control.Applicative

newtype State s a = State { runState :: s -> (a, s) }

evalState :: State s a -> s -> a
evalState = (fst .) . runState

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

instance Functor (State s) where
	fmap = (=<<) . (return .)

instance Applicative (State s) where
	pure = State . (,)
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad (State s) where
	State m >>= f = State $ \s -> let (x, s') = m s in runState (f x) s'

data STree a = Tip | Node (STree a) a (STree a) deriving Show

data Id a = Id { getId :: a } deriving Show

instance Functor Id where
	fmap = (<*>) . pure

instance Applicative Id where
	pure = Id
	Id f <*> Id x = Id $ f x

instance Functor STree where
	fmap f = getId . traverse (Id . f)

instance Foldable STree where
	foldMap f = getConst . traverse (Const . f)

instance Traversable STree where
	traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
	traverse _ _ = pure Tip

checkTree :: (Bounded a, Ord a) => STree a -> STree (a, Bool)
checkTree = ((`evalState` minBound) .) . traverse $ \x -> do
	s <- get
	put $ x `max` s
	return (x, x >= s)

sample :: STree Int
sample = Node
	(Node (Node Tip 8 Tip) 21 (Node Tip 63 Tip))
	35
	(Node (Node Tip 42 Tip) 50 (Node Tip 89 Tip))
