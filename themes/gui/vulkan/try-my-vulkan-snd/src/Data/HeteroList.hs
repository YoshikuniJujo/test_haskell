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
	Tip(..), (:.:)(..), length, StorableList(..), HeteroList(..), StoreHetero(..),
	HeteroVarList(..), pattern Singleton, singleton,
	heteroVarListToList, heteroVarListToListM,
	heteroVarListMapM, HeteroVarListMapM(..), TLength(..),
	ListToHeteroVarList(..), oneOfOne, heteroVarListIndex, heteroVarListLength,
	heteroVarListReplicateM, listToHeteroVarList', heteroVarListMap,
	V2(..), V3(..), V4(..), V5(..), V6(..),
	V12(..), V13(..), V14(..), V15(..) ) where

import Prelude hiding (length)

import Foreign.Ptr
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

-- class

class StoreHetero (ts :: [Type]) where
	storeHeteroSize :: Int -> Int
	storeHetero :: Ptr () -> HeteroList ts -> IO ()

instance StoreHetero '[] where
	storeHeteroSize sz = sz
	storeHetero _ HNil = pure ()

instance (Storable t, StoreHetero ts) => StoreHetero (t ': ts) where
	storeHeteroSize sz = storeHeteroSize @ts
		$ ((sz - 1) `div` al + 1) * al + sizeOf @t undefined
		where al = alignment @t undefined
	storeHetero p (x :..: xs) = do
		poke (castPtr p') x
		storeHetero (p' `plusPtr` sizeOf x) xs
		where p' = alignPtr p $ alignment x

infixr 5 :...:

data HeteroVarList (t :: k -> Type) (ss :: [k]) where
	HVNil :: HeteroVarList t '[]
	(:...:) :: t s -> HeteroVarList t ss -> HeteroVarList t (s ': ss)

instance Show (HeteroVarList t '[]) where show HVNil = "HVNil"

instance (Show (t s), Show (HeteroVarList t ss)) =>
	Show (HeteroVarList t (s ': ss)) where
	show (x :...: xs) = show x ++ " :...: " ++ show xs

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

heteroVarListReplicateM :: Monad m =>
	Int -> (forall a . (forall s . t s -> m a) -> m a) ->
	(forall ss . HeteroVarList t ss -> m b) -> m b
heteroVarListReplicateM 0 _ f = f HVNil
heteroVarListReplicateM n x f = x \v -> heteroVarListReplicateM (n - 1) x \vs ->
	f $ v :...: vs

heteroVarListMap ::
	(forall s . t s -> t' s) -> HeteroVarList t ss -> HeteroVarList t' ss
heteroVarListMap f = \case
	HVNil -> HVNil
	x :...: xs -> f x :...: heteroVarListMap f xs

data V2 t ss where V2 :: t s1 s2 -> V2 t '(s1, s2)
data V3 t ss where V3 :: { unV3 :: t s1 s2 s3 } -> V3 t '(s1, s2, s3)
data V4 t ss where V4 :: t s1 s2 s3 s4 -> V4 t '(s1, s2, s3, s4)
data V5 t ss where V5 :: t s1 s2 s3 s4 s5 -> V5 t '(s1, s2, s3, s4, s5)
data V6 t ss where V6 :: t s1 s2 s3 s4 s5 s6 -> V6 t '(s1, s2, s3, s4, s5, s6)

data V12 t ss where
	V12 :: t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 -> V12 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)

data V13 t ss where
	V13 :: t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 -> V13 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13)

data V14 t ss where
	V14 :: t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 -> V14 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14)

data V15 t ss where
	V15 :: t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 -> V15 t '(
		s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15
		)

deriving instance Show (t s1 s2) => Show (V2 t '(s1, s2))
deriving instance Show (t s1 s2 s3) => Show (V3 t '(s1, s2, s3))
deriving instance Show (t s1 s2 s3 s4) => Show (V4 t '(s1, s2, s3, s4))
deriving instance Show (t s1 s2 s3 s4 s5) => Show (V5 t '(s1, s2, s3, s4, s5))
deriving instance Show (t s1 s2 s3 s4 s5 s6) => Show (V6 t '(s1, s2, s3, s4, s5, s6))

deriving instance Show (t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12) =>
	Show (V12 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12))

deriving instance Show (t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13) =>
	Show (V13 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13))

deriving instance Show (t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14) =>
	Show (V14 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14))

deriving instance Show (t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15) =>
	Show (V15 t '(
		s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15
		))
