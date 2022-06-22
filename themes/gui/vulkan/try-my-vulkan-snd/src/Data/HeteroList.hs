{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroList (
	Tip(..), (:.:)(..), length, StorableList(..), HeteroList(..),
	HeteroVarList(..),
	heteroVarListToList, heteroVarListToListM,
	heteroVarListMapM, HeteroVarListMapM(..), TLength(..),
	ListToHeteroVarList(..) ) where

import Prelude hiding (length)

import Foreign.Storable
import Data.Kind

infixr 5 :.:

data a :.: b = a :.: b deriving Show

data Tip = Tip

{-# COMPLETE (:.:) #-}

class Length vs where length :: vs -> Int

instance Length Tip where length _ = 0

instance Length ts => Length (t :.: ts) where
	length (_ :.: vs) = 1 + length vs

class StorableList vs where sizeAlignments :: vs -> [(Int, Int)]

instance StorableList () where sizeAlignments _ = []
instance StorableList Tip where sizeAlignments _ = []
instance (Storable t, StorableList ts) => StorableList (t :.: ts) where
	sizeAlignments (x :.: xs) = (sizeOf x, alignment x) : sizeAlignments xs

infixr 5 :..:

data HeteroList (as :: [Type]) where
	HNil :: HeteroList '[]
	(:..:) :: a -> HeteroList as -> HeteroList (a ': as)

instance Show (HeteroList '[]) where show HNil = "HNil"

instance (Show a, Show (HeteroList as)) => Show (HeteroList (a ': as)) where
	show (x :..: xs) = show x ++ " :..: " ++ show xs

infixr 5 :...:

data HeteroVarList (t :: k -> Type) (ss :: [k]) where
	HVNil :: HeteroVarList t '[]
	(:...:) :: t s -> HeteroVarList t ss -> HeteroVarList t (s ': ss)

instance Show (HeteroVarList t '[]) where show HVNil = "HVNil"

instance (Show (t s), Show (HeteroVarList t ss)) =>
	Show (HeteroVarList t (s ': ss)) where
	show (x :...: xs) = show x ++ " :...: " ++ show xs

heteroVarListToList :: (forall s . t s -> t') -> HeteroVarList t ss -> [t']
heteroVarListToList _ HVNil = []
heteroVarListToList f (x :...: xs) = f x : heteroVarListToList f xs

heteroVarListToListM :: Applicative m =>
	(forall s . t s -> m t') -> HeteroVarList t ss -> m [t']
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
