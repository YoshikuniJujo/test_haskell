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

module Data.HeteroParList (

	-- * HeteroList

	HeteroList, pattern (:*), Id(..),

	-- * HeteroParList

	HeteroParList(..), pattern Singleton,

	-- * From/To List

	-- ** Hetero List

	ListToHeteroParList(..), heteroVarListToList, heteroVarListToListM,

	-- ** Homo List

	HomoList(..),

	-- * Index

	heteroVarListIndex, homoListIndex,

	-- * Map and ReplicateM

	heteroVarListMap, heteroVarListReplicateM,

	) where

import Data.Kind
import Data.List (genericIndex)

type HeteroList ts = HeteroParList Id ts

infixr 5 :*

pattern (:*) :: t -> HeteroList ts -> HeteroList (t ': ts)
pattern x :* xs <- Id x :...: xs where x :* xs = Id x :...: xs

newtype Id t = Id t deriving Show

infixr 5 :...:

data HeteroParList (t :: k -> Type) (ss :: [k]) where
	HNil :: HeteroParList t '[]
	(:...:) :: t s -> HeteroParList t ss -> HeteroParList t (s ': ss)

instance Show (HeteroParList t '[]) where show HNil = "HNil"

instance (Show (t s), Show (HeteroParList t ss)) =>
	Show (HeteroParList t (s ': ss)) where
	show (x :...: xs) = show x ++ " :...: " ++ show xs

instance Eq (HeteroParList t '[]) where HNil == HNil = True

instance (Eq (t s), Eq (HeteroParList t ss)) =>
	Eq (HeteroParList t (s ': ss)) where
	(x :...: xs) == (y :...: ys) = x == y && xs == ys

{-# COMPLETE Singleton #-}

pattern Singleton :: t s -> HeteroParList t '[s]
pattern Singleton x <- (x :...: HNil) where
	Singleton x = x :...: HNil

heteroVarListToList :: (forall (s :: k) . t s -> t') -> HeteroParList t ss -> [t']
heteroVarListToList _ HNil = []
heteroVarListToList f (x :...: xs) = f x : heteroVarListToList f xs

heteroVarListToListM :: Applicative m =>
	(forall (s :: k) . t s -> m t') -> HeteroParList t ss -> m [t']
heteroVarListToListM _ HNil = pure []
heteroVarListToListM f (x :...: xs) = (:) <$> f x <*> heteroVarListToListM f xs

class ListToHeteroParList ss where
	listToHeteroParList :: (forall s . t -> t' s) -> [t] -> HeteroParList t' ss

instance ListToHeteroParList '[] where
	listToHeteroParList _ [] = HNil
	listToHeteroParList _ _ = error "bad"

instance ListToHeteroParList ss => ListToHeteroParList (s ': ss) where
	listToHeteroParList f (x : xs) = f x :...: listToHeteroParList f xs
	listToHeteroParList _ _ = error "bad"

class ListToHeteroParListM ss where
	listToHeteroParListM :: Monad m =>
		(forall s . t -> m (t' s)) -> [t] -> m (HeteroParList t' ss)

instance ListToHeteroParListM '[] where
	listToHeteroParListM _ [] = pure HNil
	listToHeteroParListM _ _ = error "bad"

instance ListToHeteroParListM ss =>
	ListToHeteroParListM (s ': ss) where
	listToHeteroParListM _ [] = error "bad"
	listToHeteroParListM f (x : xs) = (:...:)
		<$> f x
		<*> listToHeteroParListM f xs

heteroVarListIndex :: Integral i => HeteroParList t ss -> i -> (forall s . t s -> a) -> a
heteroVarListIndex HNil _ _ = error "index too large"
heteroVarListIndex (x :...: _) 0 f = f x
heteroVarListIndex (_ :...: xs) i f | i > 0 = heteroVarListIndex xs (i - 1) f
heteroVarListIndex _ _ _ = error "negative index"

homoListIndex :: (HomoList a as, Integral i) => HeteroParList t as -> i -> t a
homoListIndex xs i = homoListToList xs `genericIndex` i

class HomoList (a :: k) as where
	homoListToList :: HeteroParList t as -> [t a]

instance HomoList a '[] where homoListToList HNil = []

instance HomoList a as => HomoList a (a ': as) where
	homoListToList (x :...: xs) = x : homoListToList xs

heteroVarListReplicateM :: Monad m =>
	Int -> (forall a . (forall s . t s -> m a) -> m a) ->
	(forall ss . HeteroParList t ss -> m b) -> m b
heteroVarListReplicateM 0 _ f = f HNil
heteroVarListReplicateM n x f = x \v -> heteroVarListReplicateM (n - 1) x \vs ->
	f $ v :...: vs

heteroVarListMap ::
	(forall s . t s -> t' s) -> HeteroParList t ss -> HeteroParList t' ss
heteroVarListMap f = \case
	HNil -> HNil
	x :...: xs -> f x :...: heteroVarListMap f xs
