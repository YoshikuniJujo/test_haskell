{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BinarySearchTrees where

import Prelude hiding (lookup)

import GHC.Stack (HasCallStack)
import Control.Exception
import System.IO.Unsafe

data Tree a = E | T (Tree a) a (Tree a) deriving Show

class Set s where
	empty :: s a
	insert :: Ord a => a -> s a -> s a
	member :: Ord a => a -> s a -> Bool

instance Set Tree where
	empty = E
	insert x E = T E x E
	insert x s@(T a y b)
		| x < y = T (insert x a) y b
		| x > y = T a y (insert x b)
		| otherwise = s
	member _ E = False
	member x (T a y b)
		| x < y = member x a
		| x > y = member x b
		| otherwise = True

member' :: Ord a => a -> Tree a -> Bool
member' = memberGen Nothing

memberGen :: Ord a => Maybe a -> a -> Tree a -> Bool
memberGen c x E = c == Just x
memberGen c x (T a y b)
	| x < y = memberGen c x a
	| otherwise = memberGen (Just y) x b

sampleTree, sampleTree' :: Tree Char
sampleTree = foldr insert E "bglpdnh"
sampleTree' = insert 'j' sampleTree

insert' :: Ord a => a -> Tree a -> Tree a
insert' x t = unsafePerformIO $ insertGen x t `catch` (\(_ :: IsElemEx) -> pure t)

insertGen :: Ord a => a -> Tree a -> IO (Tree a)
insertGen x E = pure $ T E x E
insertGen x (T a y b)
	| x < y = T <$> insertGen x a <*> pure y <*> pure b
	| x > y = T a y <$> insertGen x b
	| otherwise = throwIO IsElemEx

data IsElemEx = IsElemEx deriving Show

instance Exception IsElemEx

newtype ExerciseTree a = ExerciseTree { getExerciseTree :: Tree a } deriving Show

instance Set ExerciseTree where
	empty = ExerciseTree E
	insert x = ExerciseTree . insert' x . getExerciseTree
	member x = member' x . getExerciseTree

sampleExerciseTree :: ExerciseTree Char
sampleExerciseTree = ExerciseTree sampleTree

complete :: HasCallStack => a -> Int -> Tree a
complete _ 0 = E
complete x d | d > 0 = T a x a
	where a = complete x (d - 1)
complete _ d = error $ "complete: negetive depth " ++ show d

balanced :: HasCallStack => a -> Int -> Tree a
balanced x n = fst $ create2 x n

create2 :: HasCallStack => a -> Int -> (Tree a, Tree a)
create2 x 0 = (E, T E x E)
create2 x n
	| n > 0, odd n = (T a x a, T a x b)
	| n > 0, even n = (T a x b, T b x b)
	where (a, b) = create2 x ((n  - 1) `div` 2)
create2 _ n = error $ "create2: negative elements number " ++ show n

class FiniteMap m where
	emptyM :: m k v
	bind :: Ord k => k -> v -> m k v -> m k v
	lookup :: (HasCallStack, Ord k) => k -> m k v -> v

data Map k v = EM | TM (Map k v) (k, v) (Map k v) deriving Show

instance FiniteMap Map where
	emptyM = EM
	bind k v EM = TM EM (k, v) EM
	bind k v (TM a kv'@(k', _) b)
		| k < k' = TM (bind k v a) kv' b
		| k > k' = TM a kv' (bind k v b)
		| otherwise = TM a (k, v) b
	lookup _ EM = error "no element"
	lookup k (TM a (k', v) b)
		| k < k' = lookup k a
		| k > k' = lookup k b
		| otherwise = v
