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
	StorableList'(..), storeHeteroSize,

	HeteroList',
	StoreHetero'(..),
	Id(..),

	HeteroVarList(..), pattern Singleton, singleton,
	heteroVarListToList, heteroVarListToListM,
	heteroVarListMapM, HeteroVarListMapM(..), TLength(..),
	ListToHeteroVarList(..), ListToHeteroVarListM(..),
	oneOfOne, heteroVarListIndex, HeteroVarListIndex'(..), heteroVarListLength,
	heteroVarListReplicate,
	heteroVarListReplicateM, listToHeteroVarList', heteroVarListMap,
	heteroVarListZipWithM_,
	PointableHeteroMap(..), PointableToListM(..), PokableToListM(..), WithPokedToListM(..),
	PokableHeteroMap(..), WithPokedHeteroMap(..),
	StorableHeteroMap(..), StorableToListM(..),
	V2(..), V3(..), V4(..), V5(..), V6(..),
	V12(..), V13(..), V14(..), V15(..) ) where

import Prelude hiding (length)

import Foreign.Ptr
import Foreign.Storable
import Foreign.Pointable
import Data.Kind

import Foreign.Storable.PeekPoke

class StorableList' (vs :: [Type]) where sizeAlignments' :: HeteroList' vs -> [(Int, Int)]

instance StorableList' '[] where sizeAlignments' _ = []
instance (Storable t, StorableList' ts) => StorableList' (t ': ts) where
	sizeAlignments' (Id x :...: xs) = (sizeOf x, alignment x) : sizeAlignments' xs

newtype Id t = Id t deriving Show

type HeteroList' ts = HeteroVarList Id ts

class StoreHetero' (ts :: [Type]) where
	storeHetero' :: Ptr () -> HeteroList' ts -> IO ()

instance StoreHetero' '[] where
	storeHetero' _ HVNil = pure ()

instance (Storable t, StoreHetero' ts) => StoreHetero' (t ': ts) where
	storeHetero' p (Id x :...: xs) = do
		poke (castPtr p') x
		storeHetero' (p' `plusPtr` sizeOf x) xs
		where p' = alignPtr p $ alignment x

storeHeteroSize :: forall ts . StorableList' ts => HeteroList' ts -> Int
storeHeteroSize = calcSize 0 . sizeAlignments'

calcSize :: Int -> [(Int, Int)] -> Int
calcSize n [] = n
calcSize n ((sz, al) : szals) = calcSize (((n - 1) `div` al + 1) * al + sz) szals

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

data V2 t ss where V2 :: { unV2 :: t s1 s2 } -> V2 t '(s1, s2)
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

class PointableHeteroMap ns where
	pointableHeteroMap :: HeteroVarList t ns ->
		(forall n . Pointable n => t n -> a) -> [a]
	pointableHeteroMapM :: Monad m => HeteroVarList t ns ->
		(forall n . Pointable n => t n -> m a) -> m [a]

instance PointableHeteroMap '[] where
	pointableHeteroMap HVNil _ = []
	pointableHeteroMapM HVNil _ = pure []

instance (Pointable n, PointableHeteroMap ns) =>
	PointableHeteroMap (n ': ns) where
	pointableHeteroMap (x :...: xs) f = f x : pointableHeteroMap xs f
	pointableHeteroMapM (x :...: xs) f =
		(:) <$> f x <*> pointableHeteroMapM xs f

class PokableHeteroMap ns where
	pokableHeteroMap :: HeteroVarList t ns ->
		(forall n . Pokable n => t n -> a) -> [a]
	pokableHeteroMapM :: Monad m => HeteroVarList t ns ->
		(forall n . Pokable n => t n -> m a) -> m [a]

instance PokableHeteroMap '[] where
	pokableHeteroMap HVNil _ = []
	pokableHeteroMapM HVNil _ = pure []

instance (Pokable n, PokableHeteroMap ns) =>
	PokableHeteroMap (n ': ns) where
	pokableHeteroMap (x :...: xs) f = f x : pokableHeteroMap xs f
	pokableHeteroMapM (x :...: xs) f =
		(:) <$> f x <*> pokableHeteroMapM xs f

class WithPokedHeteroMap ns where
	withPokedHeteroMapM :: HeteroVarList t ns ->
		(forall n . WithPoked n => t n -> (a -> m ()) -> m ()) ->
		([a] -> m ()) -> m ()

instance WithPokedHeteroMap '[] where
	withPokedHeteroMapM HVNil _ g = g []

instance (WithPoked n, WithPokedHeteroMap ns) =>
	WithPokedHeteroMap (n ': ns) where
	withPokedHeteroMapM (x :...: xs) f g =
		f x \y -> withPokedHeteroMapM xs f \ys -> g $ y : ys

class StorableHeteroMap ns where
	storableHeteroMap :: HeteroVarList t ns ->
		(forall n . Storable n => t n -> a) -> [a]
	storableHeteroMapM :: Monad m => HeteroVarList t ns ->
		(forall n . Storable n => t n -> m a) -> m [a]

instance StorableHeteroMap '[] where
	storableHeteroMap HVNil _ = []
	storableHeteroMapM HVNil _ = pure []

instance (Storable n, StorableHeteroMap ns) =>
	StorableHeteroMap (n ': ns) where
	storableHeteroMap (x :...: xs) f = f x : storableHeteroMap xs f
	storableHeteroMapM (x :...: xs) f =
		(:) <$> f x <*> storableHeteroMapM xs f

class PointableToListM ns where
	pointableToListM :: Monad m =>
		(forall n . Pointable n => t n -> m t') -> HeteroVarList t ns -> m [t']

instance PointableToListM '[] where pointableToListM _ HVNil = pure []

instance (Storable n, PointableToListM ns) => PointableToListM (n ': ns) where
	pointableToListM f (x :...: xs) = (:) <$> f x <*> pointableToListM f xs

class PokableToListM ns where
	pokableToListM :: Monad m =>
		(forall n . Pokable n => t n -> m t') -> HeteroVarList t ns -> m [t']

instance PokableToListM '[] where pokableToListM _ HVNil = pure []

instance (Pokable n, PokableToListM ns) => PokableToListM (n ': ns) where
	pokableToListM f (x :...: xs) = (:) <$> f x <*> pokableToListM f xs

class WithPokedToListM ns where
	withPokedToListM :: Monad m =>
		(forall n . WithPoked n => t n -> m t') -> HeteroVarList t ns -> m [t']

instance WithPokedToListM '[] where withPokedToListM _ HVNil = pure []

instance (WithPoked n, WithPokedToListM ns) => WithPokedToListM (n ': ns) where
	withPokedToListM f (x :...: xs) = (:) <$> f x <*> withPokedToListM f xs

class StorableToListM ns where
	storableToListM :: Monad m =>
		(forall n . Storable n => t n -> m t') -> HeteroVarList t ns -> m [t']

instance StorableToListM '[] where storableToListM _ HVNil = pure []

instance (Storable n, StorableToListM ns) => StorableToListM (n ': ns) where
	storableToListM f (x :...: xs) = (:) <$> f x <*> storableToListM f xs
