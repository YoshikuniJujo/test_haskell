{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroParList (

	-- * Hetero List

	L, pattern (:*), Id(..),

	-- * Lengthed List
	
	LL, LL', pattern (:*.), Dummy(..), Dummies, ToDummies,

	-- * Hetero Parameter List

	PL(..), pattern Singleton, PL2, PL3, PL4,

	-- * From/To List

	-- ** Hetero List

	FromList(..),

	toList, toListM, toList3,
	ConstraintHeteroToListM(..),
	ConstraintHeteroToListM'(..),
	ConstraintHeteroToListCpsM(..),
	ConstraintHeteroToListCpsM'(..),

	-- ** Homo List

	HomoList(..),

	-- * Index

	index, homoListIndex,

	-- * Map and ReplicateM

	map, replicate, replicateM,

	) where

import Prelude hiding (map, replicate)

import GHC.TypeLits
import Data.Kind
import Data.Default
import Data.List (genericIndex)

-- Hetero List

type L as = PL Id as

{-# COMPLETE (:*) #-}

infixr 5 :*

pattern (:*) :: a -> L as -> L (a ': as)
pattern x :* xs <- Id x :** xs where x :* xs = Id x :** xs

newtype Id a = Id a deriving Show

-- Lengthed List

type LL a ds = PL (Dummy a) ds
type LL' a n = PL (Dummy a) (Dummies n)

{-# COMPLETE (:*.) #-}

infixr 5 :*.

pattern (:*.) :: a -> LL a ds -> LL a ('() ': ds)
pattern x :*. xs <- Dummy x :** xs where x :*. xs = Dummy x :** xs

newtype Dummy a (d :: ()) = Dummy a deriving Show

type family Dummies n where
	Dummies 0 = '[]
	Dummies n = '() ': Dummies (n - 1)

type family ToDummies xs where
	ToDummies '[] = '[]
	ToDummies (x ': xs) = '() ': ToDummies xs

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

type PL2 t = PL (PL t)
type PL3 t = PL (PL2 t)
type PL4 t = PL (PL3 t)

-- From/To List

class FromList ss where
	fromList :: (forall s . a -> t s) -> [a] -> PL t ss

instance FromList '[] where
	fromList _ [] = Nil
	fromList _ _ = error "bad"

instance FromList ss => FromList (s ': ss) where
	fromList f (x : xs) = f x :** fromList f xs
	fromList _ _ = error "bad"

toList :: (forall (s :: k) . t s -> a) -> PL t ss -> [a]
toList _ Nil = []
toList f (x :** xs) = f x : toList f xs

toListM :: Applicative m =>
	(forall (s :: k) . t s -> m a) -> PL t ss -> m [a]
toListM _ Nil = pure []
toListM f (x :** xs) = (:) <$> f x <*> toListM f xs

toList3 :: (forall a . t a -> b) -> PL3 t asss -> [[[b]]]
toList3 _ Nil = []
toList3 f (Nil :** xsss) = [] : toList3 f xsss
toList3 f ((Nil :** xss) :** xsss) =
	case toList3 f $ xss :** xsss of
		yss : ysss -> ([] : yss) : ysss
		[] -> error "never occur"
toList3 f (((x :** xs) :** xss) :** xsss) =
	case toList3 f $ (xs :** xss) :** xsss of
		(ys : yss) : ysss -> ((f x : ys) : yss) : ysss
		[] : _ -> error "never occur"
		[] -> error "never occur"

class HomoList (s :: k) ss where
	homoListFromList :: [t s] -> PL t ss
	homoListToList :: PL t ss -> [t s]

instance HomoList s '[] where
	homoListFromList = \case [] -> Nil; _ -> error "bad"
	homoListToList Nil = []

instance HomoList s ss => HomoList s (s ': ss) where
	homoListFromList =
		\case x : xs -> x :** homoListFromList xs; _ -> error "bad"
	homoListToList (x :** xs) = x : homoListToList xs

-- Index

index :: Integral i => PL t ss -> i -> (forall s . t s -> a) -> a
index Nil _ _ = error "index too large"
index (x :** _) 0 f = f x
index (_ :** xs) i f | i > 0 = index xs (i - 1) f
index _ _ _ = error "negative index"

homoListIndex :: forall s {t} {ss} {i} .
	(HomoList s ss, Integral i) => PL t ss -> i -> t s
homoListIndex xs i = homoListToList xs `genericIndex` i

-- Map and Replicate

map ::
	(forall s . t s -> t' s) -> PL t ss -> PL t' ss
map f = \case
	Nil -> Nil
	x :** xs -> f x :** map f xs

replicate :: Int -> (forall a . (forall s . t s -> a) -> a) ->
	(forall ss . PL t ss -> b) -> b
replicate 0 _ f = f Nil
replicate n x f = x \v -> replicate (n - 1) x \vs -> f $ v :** vs

replicateM :: Monad m =>
	Int -> (forall a . (forall s . t s -> m a) -> m a) ->
	(forall ss . PL t ss -> m b) -> m b
replicateM 0 _ f = f Nil
replicateM n x f = x \v -> replicateM (n - 1) x \vs ->
	f $ v :** vs

-- Default

instance Default (PL t '[]) where def = Nil

instance (Default (t s), Default (PL t ss)) => Default (PL t (s ': ss)) where
	def = def :** def

instance Default a => Default (Id a) where def = Id def

instance Default a => Default (Dummy a d) where def = Dummy def

-- Flatten

class ConstraintHeteroToListM c ss where
	constraintHeteroToListM :: Applicative m =>
		(forall s . c s => t s -> m a) -> PL t ss -> m [a]

instance ConstraintHeteroToListM c '[] where
	constraintHeteroToListM _ Nil = pure []

instance (c s, ConstraintHeteroToListM c ss) =>
	ConstraintHeteroToListM c (s ': ss) where
	constraintHeteroToListM f (x :** xs) =
		(:) <$> f x <*> constraintHeteroToListM @c f xs

class ConstraintHeteroToListM' c (t' :: k -> Type) (ss :: [k]) where
	constraintHeteroToListM' :: Applicative m =>
		(forall (s :: k) . c (t' s) => t s -> m a) -> PL t ss -> m [a]

instance ConstraintHeteroToListM' c t' '[] where
	constraintHeteroToListM' _ Nil = pure []

instance (c (t' s), ConstraintHeteroToListM' c t' ss) =>
	ConstraintHeteroToListM' c t' (s ': ss) where
	constraintHeteroToListM' f (x :** xs) =
		(:) <$> f x <*> constraintHeteroToListM' @_ @c @t' f xs

class ConstraintHeteroToListCpsM c ns where
	constraintHeteroToListCpsM ::
		(forall s . c s => t s -> (a -> m b) -> m b) -> PL t ns ->
		([a] -> m b) -> m b

instance ConstraintHeteroToListCpsM c '[] where
	constraintHeteroToListCpsM _ Nil g = g []

instance (c n, ConstraintHeteroToListCpsM c ns) =>
	ConstraintHeteroToListCpsM c (n ': ns) where
	constraintHeteroToListCpsM f (x :** xs) g =
		f x \y -> constraintHeteroToListCpsM @c f xs \ys -> g $ y : ys

class ConstraintHeteroToListCpsM' c (t' :: k -> Type) (ns :: [k]) where
	constraintHeteroToListCpsM' ::
		(forall (s :: k) . c (t' s) => t s -> (a -> m b) -> m b) ->
		PL t ns -> ([a] -> m b) -> m b

instance ConstraintHeteroToListCpsM' c t' '[] where
	constraintHeteroToListCpsM' _ Nil = ($ [])

instance (c (t' n), ConstraintHeteroToListCpsM' c t' ns) =>
	ConstraintHeteroToListCpsM' c t' (n ': ns) where
	constraintHeteroToListCpsM' f (x :** xs) g =
		f x \y -> constraintHeteroToListCpsM' @_ @c @t' f xs \ys -> g $ y : ys
