{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroList (
	HeteroList', Id(..),

	HeteroVarList(..), pattern Singleton, singleton,
	heteroVarListToList, heteroVarListToListM,
	heteroVarListMapM, HeteroVarListMapM(..), TLength(..),
	ListToHeteroVarList(..), ListToHeteroVarListM(..),
	oneOfOne, heteroVarListIndex, HeteroVarListIndex'(..), heteroVarListLength,
	heteroVarListReplicate,
	heteroVarListReplicateM, listToHeteroVarList', heteroVarListMap,
	heteroVarListZipWithM_,

	) where

import Data.Kind

type HeteroList' ts = HeteroVarList Id ts

newtype Id t = Id t deriving Show

infixr 5 :...:

data HeteroVarList (t :: k -> Type) (ss :: [k]) where
	HVNil :: HeteroVarList t '[]
	(:...:) :: t s -> HeteroVarList t ss -> HeteroVarList t (s ': ss)

instance Show (HeteroVarList t '[]) where show HVNil = "HVNil"

instance (Show (t s), Show (HeteroVarList t ss)) =>
	Show (HeteroVarList t (s ': ss)) where
	show (x :...: xs) = show x ++ " :...: " ++ show xs

instance Eq (HeteroVarList t '[]) where HVNil == HVNil = True

instance (Eq (t s), Eq (HeteroVarList t ss)) =>
	Eq (HeteroVarList t (s ': ss)) where
	(x :...: xs) == (y :...: ys) = x == y && xs == ys

{-# COMPLETE Singleton #-}

pattern Singleton :: t s -> HeteroVarList t '[s]
pattern Singleton x <- (x :...: HVNil) where
	Singleton x = x :...: HVNil

singleton :: t s -> HeteroVarList t '[s]
singleton = (:...: HVNil)

heteroVarListToList :: (forall (s :: k) . t s -> t') -> HeteroVarList t ss -> [t']
heteroVarListToList _ HVNil = []
heteroVarListToList f (x :...: xs) = f x : heteroVarListToList f xs

heteroVarListToListM :: Applicative m =>
	(forall (s :: k) . t s -> m t') -> HeteroVarList t ss -> m [t']
heteroVarListToListM _ HVNil = pure []
heteroVarListToListM f (x :...: xs) = (:) <$> f x <*> heteroVarListToListM f xs

heteroVarListMapM :: Applicative m =>
	(forall (s :: k) . t s -> m (t' s)) -> HeteroVarList t ss -> m (HeteroVarList t' ss)
heteroVarListMapM _ HVNil = pure HVNil
heteroVarListMapM f (x :...: xs) = (:...:) <$> f x <*>  heteroVarListMapM f xs

class HeteroVarListMapM ss fss where
	heteroVarListMapM' :: Applicative m =>
		(forall a b c . t '(a, b, c) -> m (t' a)) -> HeteroVarList t ss ->
		m (HeteroVarList t' fss)

instance HeteroVarListMapM '[] '[] where heteroVarListMapM' _ HVNil = pure HVNil

instance HeteroVarListMapM ss fss =>
	HeteroVarListMapM ('(x, y, z) ': ss) (x ': fss) where
	heteroVarListMapM' f (x :...: xs) = (:...:) <$> f x <*> heteroVarListMapM' f xs

heteroVarListLength :: HeteroVarList t ss -> Int
heteroVarListLength HVNil = 0
heteroVarListLength (_ :...: xs) = 1 + heteroVarListLength xs

class TLength ts where tLength :: Num n => n

instance TLength '[] where tLength = 0

instance TLength ts => TLength (t ': ts) where tLength = 1 + tLength @ts

class ListToHeteroVarList ss where
	listToHeteroVarList :: (forall s . t -> t' s) -> [t] -> HeteroVarList t' ss

instance ListToHeteroVarList '[] where
	listToHeteroVarList _ [] = HVNil
	listToHeteroVarList _ _ = error "bad"

instance ListToHeteroVarList ss => ListToHeteroVarList (s ': ss) where
	listToHeteroVarList f (x : xs) = f x :...: listToHeteroVarList f xs
	listToHeteroVarList _ _ = error "bad"

class ListToHeteroVarListM ss where
	listToHeteroVarListM :: Monad m =>
		(forall s . t -> m (t' s)) -> [t] -> m (HeteroVarList t' ss)

instance ListToHeteroVarListM '[] where
	listToHeteroVarListM _ [] = pure HVNil
	listToHeteroVarListM _ _ = error "bad"

instance ListToHeteroVarListM ss =>
	ListToHeteroVarListM (s ': ss) where
	listToHeteroVarListM _ [] = error "bad"
	listToHeteroVarListM f (x : xs) = (:...:)
		<$> f x
		<*> listToHeteroVarListM f xs

oneOfOne :: HeteroVarList t '[s] -> t s
oneOfOne (x :...: HVNil) = x

listToHeteroVarList' :: (forall s . t -> t' s) -> [t] ->
	(forall ss . HeteroVarList t' ss -> a) -> a
listToHeteroVarList' _ [] g = g HVNil
listToHeteroVarList' f (x : xs) g = listToHeteroVarList' f xs \ys -> g $ f x :...: ys

heteroVarListIndex :: Integral i => HeteroVarList t ss -> i -> (forall s . t s -> a) -> a
heteroVarListIndex HVNil _ _ = error "index too large"
heteroVarListIndex (x :...: _) 0 f = f x
heteroVarListIndex (_ :...: xs) i f | i > 0 = heteroVarListIndex xs (i - 1) f
heteroVarListIndex _ _ _ = error "negative index"

class HeteroVarListIndex' (a :: k) ss where
	heteroVarListIndex' :: Integral i => HeteroVarList t ss -> i -> t a

instance HeteroVarListIndex' a '[] where
	heteroVarListIndex' HVNil _ = error "index too large"

instance HeteroVarListIndex' a ss => HeteroVarListIndex' a (a ': ss) where
	heteroVarListIndex' (x :...: _) n | n < 1 = x
	heteroVarListIndex' (_ :...: xs) n = heteroVarListIndex' xs (n - 1)

heteroVarListReplicateM :: Monad m =>
	Int -> (forall a . (forall s . t s -> m a) -> m a) ->
	(forall ss . HeteroVarList t ss -> m b) -> m b
heteroVarListReplicateM 0 _ f = f HVNil
heteroVarListReplicateM n x f = x \v -> heteroVarListReplicateM (n - 1) x \vs ->
	f $ v :...: vs

heteroVarListReplicate ::
	Int -> t s -> (forall ss . ListToHeteroVarList ss => HeteroVarList t ss -> IO a) -> IO a
heteroVarListReplicate 0 _ f = f HVNil
heteroVarListReplicate n x f = heteroVarListReplicate (n - 1) x \xs -> f $ x :...: xs

heteroVarListMap ::
	(forall s . t s -> t' s) -> HeteroVarList t ss -> HeteroVarList t' ss
heteroVarListMap f = \case
	HVNil -> HVNil
	x :...: xs -> f x :...: heteroVarListMap f xs

heteroVarListZipWithM_ :: Monad m => (forall (s :: k) (s' :: k') . t s -> t' s' -> m a) ->
	HeteroVarList t ss -> HeteroVarList t' ss' -> m ()
heteroVarListZipWithM_ _ HVNil _ = pure ()
heteroVarListZipWithM_ _ _ HVNil = pure ()
heteroVarListZipWithM_ op (x :...: xs) (y :...: ys) =
	op x y >> heteroVarListZipWithM_ op xs ys
