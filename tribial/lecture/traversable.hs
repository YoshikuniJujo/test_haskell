{-# LANGUAGE ExistentialQuantification #-}

import Data.Tree

msequence :: Applicative m => Maybe (m a) -> m (Maybe a)
msequence Nothing = pure Nothing
msequence (Just m) = fmap Just m

lsequence :: Applicative m => [m a] -> m [a]
lsequence [] = pure []
lsequence (m : ms) = (:) <$> m <*> lsequence ms

{-

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

instance Foldable BinTree where
	foldMap f = getConst . traverse (Const . f)

instance Functor BinTree where
	fmap f = getId . traverse (Id . f)

instance Traversable BinTree where

-}

newtype Id a = Id { getId :: a }

instance Functor Id where
	fmap f (Id x) = Id (f x)

instance Applicative Id where
	pure = Id
	Id f <*> Id x = Id (f x)

newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
	fmap _ (Const x) = Const x

instance Monoid m => Applicative (Const m) where
	pure _ = Const mempty
	Const x <*> Const y = Const $ x `mappend` y


data App a = App (String -> a) String

instance Functor App where
	fmap f (App g x) = App (f . g) x

instance Applicative App where
--	pure x = App (const x) (show x)
--	App f a <*> App x b = 

-- (+) <$> x <*> y
-- App (+) (App x (App y N))

data Count a = Count Int a deriving Show

instance Functor Count where
	fmap f (Count n x) = Count n $ f x

instance Applicative Count where
	pure = Count 0
	Count m f <*> Count n x = Count (m + n) $ f x

data Store s a = Store s a deriving Show

instance Functor (Store s) where
	fmap f (Store s x) = Store s $ f x

instance Monoid s => Applicative (Store s) where
	pure = Store mempty
	Store s f <*> Store t x = Store (s `mappend` t) $ f x

showStore :: Show a => a -> Store [String] a
showStore x = Store [show x] x
