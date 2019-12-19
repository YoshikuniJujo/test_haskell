{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GenericTree where

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

(<--) :: Tree a -> (a -> Tree b) -> Tree b
Leaf x <-- f = f x
Node l r <-- f = Node (l <-- f) (r <-- f)

instance Functor Tree where
	fmap f (Leaf x) = Leaf $ f x
	fmap f (Node l r) = Node (f <$> l) (f <$> r)

instance Applicative Tree where
	pure = Leaf
	mf <*> mx = mf <-- \f -> mx <-- \x -> pure (f x)

instance Monad Tree where (>>=) = (<--)

monad1 :: Tree Int
monad1 = Node (Node (Leaf 123) (Node (Leaf 456) (Leaf 456))) (Node (Leaf 789) (Leaf 987))

funF, funG :: Int -> Tree Int
funF x = Node (Leaf x) (Node (Leaf $ x * 2) (Leaf $ x * 3))
funG x = Node (Node (Leaf $ x + 10000) (Leaf $ x + 20000)) (Leaf $ x + 30000)
