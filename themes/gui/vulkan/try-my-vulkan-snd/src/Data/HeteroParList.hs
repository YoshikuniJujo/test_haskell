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

	ListToHeteroParList(..), heteroParListToList, heteroParListToListM,

	-- ** Homo List

	HomoList(..),

	-- * Index

	heteroParListIndex, homoListIndex,

	-- * Map and ReplicateM

	heteroParListMap, heteroParListReplicateM,

	) where

import Data.Kind
import Data.List (genericIndex)

-- HeteroList

type HeteroList ts = HeteroParList Id ts

infixr 5 :*

pattern (:*) :: t -> HeteroList ts -> HeteroList (t ': ts)
pattern x :* xs <- Id x :...: xs where x :* xs = Id x :...: xs

newtype Id t = Id t deriving Show

-- HeteroParList

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

-- From/To List

class ListToHeteroParList ss where
	listToHeteroParList :: (forall s . t -> t' s) -> [t] -> HeteroParList t' ss

instance ListToHeteroParList '[] where
	listToHeteroParList _ [] = HNil
	listToHeteroParList _ _ = error "bad"

instance ListToHeteroParList ss => ListToHeteroParList (s ': ss) where
	listToHeteroParList f (x : xs) = f x :...: listToHeteroParList f xs
	listToHeteroParList _ _ = error "bad"

heteroParListToList :: (forall (s :: k) . t s -> t') -> HeteroParList t ss -> [t']
heteroParListToList _ HNil = []
heteroParListToList f (x :...: xs) = f x : heteroParListToList f xs

heteroParListToListM :: Applicative m =>
	(forall (s :: k) . t s -> m t') -> HeteroParList t ss -> m [t']
heteroParListToListM _ HNil = pure []
heteroParListToListM f (x :...: xs) = (:) <$> f x <*> heteroParListToListM f xs

class HomoList (a :: k) as where
	homoListToList :: HeteroParList t as -> [t a]

instance HomoList a '[] where homoListToList HNil = []

instance HomoList a as => HomoList a (a ': as) where
	homoListToList (x :...: xs) = x : homoListToList xs

-- Index

heteroParListIndex :: Integral i => HeteroParList t ss -> i -> (forall s . t s -> a) -> a
heteroParListIndex HNil _ _ = error "index too large"
heteroParListIndex (x :...: _) 0 f = f x
heteroParListIndex (_ :...: xs) i f | i > 0 = heteroParListIndex xs (i - 1) f
heteroParListIndex _ _ _ = error "negative index"

homoListIndex :: (HomoList a as, Integral i) => HeteroParList t as -> i -> t a
homoListIndex xs i = homoListToList xs `genericIndex` i

-- Map and ReplicateM

heteroParListMap ::
	(forall s . t s -> t' s) -> HeteroParList t ss -> HeteroParList t' ss
heteroParListMap f = \case
	HNil -> HNil
	x :...: xs -> f x :...: heteroParListMap f xs

heteroParListReplicateM :: Monad m =>
	Int -> (forall a . (forall s . t s -> m a) -> m a) ->
	(forall ss . HeteroParList t ss -> m b) -> m b
heteroParListReplicateM 0 _ f = f HNil
heteroParListReplicateM n x f = x \v -> heteroParListReplicateM (n - 1) x \vs ->
	f $ v :...: vs
