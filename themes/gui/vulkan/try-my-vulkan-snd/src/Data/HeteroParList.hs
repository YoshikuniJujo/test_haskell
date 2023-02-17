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

	-- * Hetero List

	L, pattern (:*), Id(..),

	-- * Hetero Parameter List

	PL(..), pattern Singleton,

	-- * From/To List

	-- ** Hetero List

	FromList(..), heteroParListToList, heteroParListToListM,

	-- ** Homo List

	HomoList(..),

	-- * Index

	heteroParListIndex, homoListIndex,

	-- * Map and ReplicateM

	heteroParListMap, heteroParListReplicateM,

	) where

import Data.Kind
import Data.List (genericIndex)

-- Hetero List

type L ts = PL Id ts

infixr 5 :*

pattern (:*) :: t -> L ts -> L (t ': ts)
pattern x :* xs <- Id x :** xs where x :* xs = Id x :** xs

newtype Id t = Id t deriving Show

-- Hetero Parameter List

infixr 5 :**

data PL (t :: k -> Type) (ss :: [k]) where
	Nil :: PL t '[]
	(:**) :: t s -> PL t ss -> PL t (s ': ss)

instance Show (PL t '[]) where show Nil = "Nil"

instance (Show (t s), Show (PL t ss)) =>
	Show (PL t (s ': ss)) where
	show (x :** xs) = show x ++ " :** " ++ show xs

instance Eq (PL t '[]) where Nil == Nil = True

instance (Eq (t s), Eq (PL t ss)) =>
	Eq (PL t (s ': ss)) where
	(x :** xs) == (y :** ys) = x == y && xs == ys

{-# COMPLETE Singleton #-}

pattern Singleton :: t s -> PL t '[s]
pattern Singleton x <- (x :** Nil) where
	Singleton x = x :** Nil

-- From/To List

class FromList ss where
	listToHeteroParList :: (forall s . t -> t' s) -> [t] -> PL t' ss

instance FromList '[] where
	listToHeteroParList _ [] = Nil
	listToHeteroParList _ _ = error "bad"

instance FromList ss => FromList (s ': ss) where
	listToHeteroParList f (x : xs) = f x :** listToHeteroParList f xs
	listToHeteroParList _ _ = error "bad"

heteroParListToList :: (forall (s :: k) . t s -> t') -> PL t ss -> [t']
heteroParListToList _ Nil = []
heteroParListToList f (x :** xs) = f x : heteroParListToList f xs

heteroParListToListM :: Applicative m =>
	(forall (s :: k) . t s -> m t') -> PL t ss -> m [t']
heteroParListToListM _ Nil = pure []
heteroParListToListM f (x :** xs) = (:) <$> f x <*> heteroParListToListM f xs

class HomoList (a :: k) as where
	homoListToList :: PL t as -> [t a]

instance HomoList a '[] where homoListToList Nil = []

instance HomoList a as => HomoList a (a ': as) where
	homoListToList (x :** xs) = x : homoListToList xs

-- Index

heteroParListIndex :: Integral i => PL t ss -> i -> (forall s . t s -> a) -> a
heteroParListIndex Nil _ _ = error "index too large"
heteroParListIndex (x :** _) 0 f = f x
heteroParListIndex (_ :** xs) i f | i > 0 = heteroParListIndex xs (i - 1) f
heteroParListIndex _ _ _ = error "negative index"

homoListIndex :: (HomoList a as, Integral i) => PL t as -> i -> t a
homoListIndex xs i = homoListToList xs `genericIndex` i

-- Map and ReplicateM

heteroParListMap ::
	(forall s . t s -> t' s) -> PL t ss -> PL t' ss
heteroParListMap f = \case
	Nil -> Nil
	x :** xs -> f x :** heteroParListMap f xs

heteroParListReplicateM :: Monad m =>
	Int -> (forall a . (forall s . t s -> m a) -> m a) ->
	(forall ss . PL t ss -> m b) -> m b
heteroParListReplicateM 0 _ f = f Nil
heteroParListReplicateM n x f = x \v -> heteroParListReplicateM (n - 1) x \vs ->
	f $ v :** vs
