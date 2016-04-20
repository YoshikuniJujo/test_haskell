{-# LANGUAGE MonadComprehensions #-}

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

check1 :: Ord a => a -> State a (a, Bool)
check1 x = do
	s <- get
	put $ x `max` s
	return (x, x >= s)

checkTreeS :: Ord a => STree a -> State a (STree (a, Bool))
checkTreeS (Node l x r) = Node <$> checkTreeS l <*> check1 x <*> checkTreeS r
checkTreeS _ = pure Tip

checkTree :: (Bounded a, Ord a) => STree a -> STree (a, Bool)
checkTree = (`evalState` minBound) . checkTreeS

ttraverse :: Applicative f => (a -> f b) -> STree a -> f (STree b)
ttraverse f (Node l x r) = Node <$> ttraverse f l <*> f x <*> ttraverse f r
ttraverse _ _ = pure Tip

checkTree' :: (Bounded a, Ord a) => STree a -> STree (a, Bool)
checkTree' = (`evalState` minBound) . ttraverse check1

ltraverse :: Applicative f => (a -> f b) -> [] a -> f ([] b)
ltraverse f (x : xs) = (:) <$> f x <*> ltraverse f xs
ltraverse _ _ = pure []

checkList :: (Bounded a, Ord a) => [a] -> [(a, Bool)]
checkList = (`evalState` minBound) . ltraverse check1
