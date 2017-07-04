{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Maybe
import Data.Tree

class MFunctor t where
	hoist :: Monad m => (forall a . m a -> n a) -> t m b -> t n b

instance MFunctor (EitherT e) where
	hoist _ (LeftT e) = LeftT e
	hoist f (RightT m) = RightT $ f m

data EitherT e m a = LeftT e | RightT (m a) deriving Show

some :: EitherT String [] Int
some = RightT [1, 2, 3]

class MFunctorR1 t where
	hoistR1 :: Monad m => (m a -> n a) -> t m a -> t n a

instance MFunctorR1 (EitherT e) where
	hoistR1 _ (LeftT e) = LeftT e
	hoistR1 f (RightT m) = RightT $ f m

foo :: [Int] -> Maybe Int
foo [] = Nothing
foo (n : _) = Just $ n * 2

height :: Tree a -> Int
height (Node _ []) = 1
height (Node _ ts) = 1 + maximum (map height ts)

bar :: (forall a . Tree a -> Int) -> Tree b -> Tree c -> Int
bar f t1 t2 = f t1 + f t2
