{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroParList (

	-- * Hetero List

	L, pattern (:*), Id(..),

	-- * Lengthed List
	
	LL, LL', pattern (:*.), Dummy(..), Dummies, ToDummies,

	-- * Hetero Parameter List

	PL(..), PL2, PL3, PL4,
	pattern Singleton, pattern Singleton2,
	pattern Singleton3, pattern Singleton4,

	-- * From/To List

	-- ** Hetero List

	-- *** fromList

	FromList(..),

	-- *** toList

	toList, toList2, toList3,
	toListM, toListM2, toListM3, toListM_, toListM2_, toListM3_,
	ToListT2(..), ToListT3(..),

	-- *** zipToList

	zipToList, zip3ToList, zip4ToList,

	-- *** zipList

--	zipList, zipList2, zipList3,
--	zipListM, zipListM2, zipListM3, zipListM_, zipListM2_, zipListM3_,

	-- *** with constraint

	ToListWithC(..), ToListWithC2(..),
	ZipListWithC(..), ZipListWithC2(..), ZipListWithC3(..),

	ToListWithCM(..), ToListWithCM'(..),
	ToListWithCCpsM(..), ToListWithCCpsM'(..), withListWithCCpsM',

	-- ** Homo List

	-- *** Homo List

	HomoList(..),

	-- *** Homo List With Num

	HomoListN(..), Num, tnum,

	-- * Index

	TypeIndex(..), index, homoListIndex,

	-- * Map and ReplicateM

	map, mapM, mapM_, MapM'(..),
	Rep(..), RepM(..), replicate, replicateM, replicateMWithI

	) where

import Prelude hiding (map, mapM, mapM_, replicate, Num)

import GHC.TypeLits
import Data.Kind
import Data.Proxy
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

pattern Singleton2 :: t s -> PL2 t '[ '[s]]
pattern Singleton2 x <- Singleton (Singleton x) where
	Singleton2 x = Singleton $ Singleton x

pattern Singleton3 :: t s -> PL3 t '[ '[ '[s]]]
pattern Singleton3 x <- Singleton (Singleton2 x) where
	Singleton3 x = Singleton $ Singleton2 x

pattern Singleton4 :: t s -> PL4 t '[ '[ '[ '[s]]]]
pattern Singleton4 x <- Singleton (Singleton3 x) where
	Singleton4 x = Singleton $ Singleton3 x

type PL2 t = PL (PL t)
type PL3 t = PL (PL2 t)
type PL4 t = PL (PL3 t)

-- From/To List

class FromList (ss :: [k]) where
	fromList :: (forall (s :: k) . a -> t s) -> [a] -> PL t ss

instance FromList '[] where
	fromList _ [] = Nil
	fromList _ _ = error "bad"

instance FromList ss => FromList (s ': ss) where
	fromList f (x : xs) = f x :** fromList f xs
	fromList _ _ = error "bad"

toList :: (forall (s :: k) . t s -> a) -> PL t ss -> [a]
toList _ Nil = []
toList f (x :** xs) = f x : toList f xs
 
toList2 :: (forall (s :: k) . t s -> a) -> PL2 t sss -> [[a]]
toList2 f = toList $ toList f

toList3 :: (forall (s :: k) . t s -> a) -> PL3 t ssss -> [[[a]]]
toList3 f = toList $ toList2 f

toListM :: Applicative m => (forall (s :: k) . t s -> m a) -> PL t ss -> m [a]
toListM _ Nil = pure []
toListM f (x :** xs) = (:) <$> f x <*> toListM f xs

toListM2 :: Applicative m =>
	(forall (s :: k) . t s -> m a) -> PL2 t sss -> m [[a]]
toListM2 f = toListM $ toListM f

toListM3 :: Applicative m =>
	(forall (s :: k) . t s -> m a) -> PL3 t sss -> m [[[a]]]
toListM3 f = toListM $ toListM2 f

toListM_ :: Applicative m => (forall (s :: k) . t s -> m a) -> PL t ss -> m ()
toListM_ _ Nil = pure ()
toListM_ f (x :** xs) = f x *> toListM_ f xs

toListM2_ :: Applicative m => (forall (s :: k) . t s -> m a) -> PL2 t ss -> m ()
toListM2_ f = toListM_ $ toListM_ f

toListM3_ :: Applicative m => (forall (s :: k) . t s -> m a) -> PL3 t ss -> m ()
toListM3_ f = toListM_ $ toListM2_ f

class ToListT2 k1 k2 (ss :: [(k1, k2)]) where
	toListT2 ::
		(forall (s1 :: k1) (s2 :: k2) . t '(s1, s2) -> a) ->
		PL t ss -> [a]

instance ToListT2 k1 k2 '[] where toListT2 _ Nil = []

instance ToListT2 k1 k2 ss => ToListT2 k1 k2 ('(s1, s2) ': ss) where
	toListT2 f (x :** xs) = f x : toListT2 f xs

class ToListT3 k1 k2 k3 (ss :: [(k1, k2, k3)]) where
	toListT3 ::
		(forall (s1 :: k1) (s2 :: k2) (s3 :: k3) .
			t '(s1, s2, s3) -> a) -> PL t ss -> [a]

instance ToListT3 k1 k2 k3 '[] where toListT3 _ Nil = []

instance ToListT3 k1 k2 k3 ss => ToListT3 k1 k2 k3 ('(s1, s2, s3) ': ss) where
	toListT3 f (x :** xs) = f x : toListT3 f xs

zipToList :: (forall (s :: k) (s' :: k') . t s -> t' s' -> a) ->
	PL t ss -> PL t' ss' -> [a]
zipToList _ Nil _ = []
zipToList _ _ Nil = []
zipToList f (x :** xs) (y :** ys) = f x y : zipToList f xs ys

zip3ToList :: (forall (s1 :: k1) (s2 :: k2) (s3 :: k3) .
	t1 s1 -> t2 s2 -> t3 s3 -> a) ->
	PL t1 ss1 -> PL t2 ss2 -> PL t3 ss3 -> [a]
zip3ToList _ Nil _ _ = []
zip3ToList _ _ Nil _ = []
zip3ToList _ _ _ Nil = []
zip3ToList f (x :** xs) (y :** ys) (z :** zs) = f x y z : zip3ToList f xs ys zs

zip4ToList :: (forall (s1 :: k1) (s2 :: k2) (s3 :: k3) (s4 :: k4) .
	t1 s1 -> t2 s2 -> t3 s3 -> t4 s4 -> a) ->
	PL t1 ss1 -> PL t2 ss2 -> PL t3 ss3 -> PL t4 ss4 -> [a]
zip4ToList _ Nil _ _ _ = []
zip4ToList _ _ Nil _ _ = []
zip4ToList _ _ _ Nil _ = []
zip4ToList _ _ _ _ Nil = []
zip4ToList f (x :** xs) (y :** ys) (z :** zs) (w :** ws) =
	f x y z w : zip4ToList f xs ys zs ws

zipList :: (forall (s :: k) . t s -> t' s -> a) ->
	PL t ss -> PL t' ss -> [a]
zipList _ Nil Nil = []
zipList f (x :** xs) (y :** ys) = f x y : zipList f xs ys

zipList2 :: (forall (s :: k) . t s -> t' s -> a) ->
	PL2 t ss -> PL2 t' ss -> [[a]]
zipList2 f = zipList $ zipList f

zipList3 :: (forall (s :: k) . t s -> t' s -> a) ->
	PL3 t ss -> PL3 t' ss -> [[[a]]]
zipList3 f = zipList $ zipList2 f

zipListM :: Applicative m =>
	(forall (s :: k) . t s -> t' s -> m a) ->
	PL t ss -> PL t' ss -> m [a]
zipListM _ Nil Nil = pure []
zipListM f (x :** xs) (y :** ys) = (:) <$> f x y <*> zipListM f xs ys

zipListM2 :: Applicative m =>
	(forall (s :: k) . t s -> t' s -> m a) ->
	PL2 t ss -> PL2 t' ss -> m [[a]]
zipListM2 f = zipListM $ zipListM f

zipListM3 :: Applicative m =>
	(forall (s :: k) . t s -> t' s -> m a) ->
	PL3 t ss -> PL3 t' ss -> m [[[a]]]
zipListM3 f = zipListM $ zipListM2 f

zipListM_ :: Applicative m =>
	(forall (s :: k) . t s -> t' s -> m a) ->
	PL t ss -> PL t' ss -> m ()
zipListM_ _ Nil Nil = pure ()
zipListM_ f (x :** xs) (y :** ys) = f x y *> zipListM_ f xs ys

zipListM2_ :: Applicative m =>
	(forall (s :: k) . t s -> t' s -> m a) ->
	PL2 t ss -> PL2 t' ss -> m ()
zipListM2_ f = zipListM_ $ zipListM_ f

zipListM3_ :: Applicative m =>
	(forall (s :: k) . t s -> t' s -> m a) ->
	PL3 t ss -> PL3 t' ss -> m ()
zipListM3_ f = zipListM_ $ zipListM2_ f

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

-- Homo List With Num

class HomoListN (n :: Num k) where
	type Replicate n s :: [Type]
	homoListNFromList :: [t s] -> PL t (Replicate n s)
	mapHomoListNM :: Monad m => (t a -> m (u b)) ->
		PL t (Replicate n a) -> m (PL u (Replicate n b))
	mapHomoListNMWithI :: Monad m => Int -> (Int -> t a -> m (u b)) ->
		PL t (Replicate n a) -> m (PL u (Replicate n b))
	zipWithHomoListNM :: Monad m => (t a -> u b -> m (v c)) ->
		PL t (Replicate n a) -> PL u (Replicate n b) ->
		m (PL v (Replicate n c))
	zipWithHomoListNM_ :: Monad m => (t a -> u b -> m c) ->
		PL t (Replicate n a) -> PL u (Replicate n b) ->
		m ()

instance HomoListN '[] where
	type Replicate '[] s = '[]
	homoListNFromList = \case [] -> Nil; _ -> error "bad"
	mapHomoListNM _ Nil = pure Nil
	mapHomoListNMWithI _ _ Nil = pure Nil
	zipWithHomoListNM _ Nil Nil = pure Nil
	zipWithHomoListNM_ _ Nil Nil = pure ()

instance HomoListN ds => HomoListN (d ': ds) where
	type Replicate (d ': ds) s = s ': Replicate ds s
	homoListNFromList = \case
		(x : xs) -> x :** (homoListNFromList @_ @ds xs); _ -> error "bad"
	mapHomoListNM :: forall t a u b m . Monad m =>
		(t a -> m (u b)) -> PL t (Replicate (d ': ds) a) ->
		m (PL u (Replicate (d ': ds) b))
	mapHomoListNM f (x :** xs) = (:**) <$> f x <*> mapHomoListNM @_ @ds f xs
	mapHomoListNMWithI :: forall t a u b m . Monad m =>
		Int -> (Int -> t a -> m (u b)) ->
		PL t (Replicate (d ': ds) a) ->
		m (PL u (Replicate (d ': ds) b))
	mapHomoListNMWithI i f (x :** xs) =
		(:**) <$> f i x <*> mapHomoListNMWithI @_ @ds (i + 1) f xs
	zipWithHomoListNM a (x :** xs) (y :** ys) =
		(:**) <$> a x y <*> zipWithHomoListNM @_ @ds a xs ys
	zipWithHomoListNM_ a (x :** xs) (y :** ys) =
		a x y >> zipWithHomoListNM_ @_ @ds a xs ys

type Num a = [a]

tnum :: [a] -> (forall (n :: Num ()) . HomoListN n => Proxy n -> b) -> b
tnum [] f = f (Proxy :: Proxy '[])
tnum (_ : xs) f = tnum xs \(Proxy :: Proxy n) -> f (Proxy :: Proxy ('() ': n))

-- Index

class TypeIndex (obj :: k) objs where typeIndex :: PL t objs -> t obj
instance TypeIndex obj (obj ': objs) where typeIndex (ln :** _lns) = ln

instance {-# OVERLAPPABLE #-} TypeIndex obj objs =>
	TypeIndex obj (obj' ': objs) where
	typeIndex (_ :** lns) = typeIndex @_ @obj @objs lns

index :: Integral i => PL t ss -> i -> (forall s . t s -> a) -> a
index Nil _ _ = error "index too large"
index (x :** _) 0 f = f x
index (_ :** xs) i f | i > 0 = index xs (i - 1) f
index _ _ _ = error "negative index"

homoListIndex :: forall s {t} {ss} {i} .
	(HomoList s ss, Integral i) => PL t ss -> i -> t s
homoListIndex xs i = homoListToList xs `genericIndex` i

-- Map and Replicate

map :: (forall s . t s -> t' s) -> PL t ss -> PL t' ss
map f = \case
	Nil -> Nil
	x :** xs -> f x :** map f xs

mapM :: Applicative m => (forall s . t s -> m (t' s)) -> PL t ss -> m (PL t' ss)
mapM f = \case
	Nil -> pure Nil
	x :** xs -> (:**) <$> f x <*> mapM f xs

mapM_ :: Applicative m => (forall s . t s -> m a) -> PL t ss -> m ()
mapM_ f = \case
	Nil -> pure ()
	x :** xs -> f x *> mapM_ f xs

class MapM' (f :: k -> k') ss where
	type Ss' f ss :: [k']
	mapM' :: Applicative m => (forall s . t s -> m (t' (f s))) ->
		PL t ss -> m (PL t' (Ss' f ss))

instance MapM' f '[] where type Ss' f '[] = '[]; mapM' _ Nil = pure Nil

instance MapM' f ss => MapM' f (s ': ss) where
	type Ss' f (s ': ss) = f s ': Ss' f ss
	mapM' g (x :** xs) = (:**) <$> g x <*> mapM' g xs

class Rep n where
	rep :: (forall a . (forall s . t s -> a) -> a) ->
		(forall ss . PL t ss -> b) -> b

instance Rep 0 where rep _ f = f Nil

instance {-# OVERLAPPABLE #-} Rep (n - 1) => Rep n where
	rep x f = x \v -> rep @(n - 1) x \vs -> f $ v :** vs

class RepM n where
	repM :: (forall a . (forall s . t s -> m a) -> m a) ->
		(forall ss . PL t ss -> m b) -> m b

instance RepM 0 where repM _ f = f Nil

instance {-# OVERLAPPABLE #-} RepM (n - 1) => RepM n where
	repM x f = x \v -> repM @(n - 1) x \vs -> f $ v :** vs

instance RepM '[] where repM _ f = f Nil

instance {-# OVERLAPPABLE #-} RepM n => RepM ('() ': n) where
	repM x f = x \v -> repM @n x \vs -> f $ v :** vs

replicate :: Int -> (forall a . (forall s . t s -> a) -> a) ->
	(forall ss . PL t ss -> b) -> b
replicate 0 _ f = f Nil
replicate n x f = x \v -> replicate (n - 1) x \vs -> f $ v :** vs

replicateM :: Int -> (forall a . (forall s . t s -> m a) -> m a) ->
	(forall ss . PL t ss -> m b) -> m b
replicateM 0 _ f = f Nil
replicateM n x f = x \v -> replicateM (n - 1) x \vs ->
	f $ v :** vs

replicateMWithI :: Int -> (forall a . Int -> (forall s . t s -> m a) -> m a) ->
	(forall ss . PL t ss -> m b) -> m b
replicateMWithI = go 0
	where
	go :: Int -> Int -> (forall a . Int -> (forall s . t s -> m a) -> m a) ->
		(forall ss . PL t ss -> m b) -> m b
	go _ 0 _ f = f Nil
	go i n x f = x i \v -> go (i + 1) (n - 1) x \vs -> f $ v :** vs

-- Default

instance Default (PL t '[]) where def = Nil

instance (Default (t s), Default (PL t ss)) => Default (PL t (s ': ss)) where
	def = def :** def

instance Default a => Default (Id a) where def = Id def

instance Default a => Default (Dummy a d) where def = Dummy def

-- Flatten

class ToListWithC c ss where
	toListWithC :: (forall s . c s => t s -> a) -> PL t ss -> [a]

instance ToListWithC c '[] where toListWithC _ Nil = []

instance (c s, ToListWithC c ss) => ToListWithC c (s ': ss) where
	toListWithC f (x :** xs) = f x : toListWithC @c f xs

class ToListWithC2 c sss where
	toListWithC2 :: (forall s . c s => t s -> a) -> PL2 t sss -> [[a]]

instance ToListWithC2 c '[] where toListWithC2 _ Nil = []

instance (ToListWithC c ss, ToListWithC2 c sss) =>
	ToListWithC2 c (ss ': sss) where
	toListWithC2 f (xs :** xss) =
		toListWithC @c f xs : toListWithC2 @c f xss

class ZipListWithC c ss where
	zipListWithC :: (forall s . c s => t s -> t' s -> a) ->
		PL t ss -> PL t' ss -> [a]

instance ZipListWithC c '[] where zipListWithC _ Nil Nil = []

instance (c s, ZipListWithC c ss) => ZipListWithC c (s ': ss) where
	zipListWithC f (x :** xs) (y :** ys) = f x y : zipListWithC @c f xs ys

class ZipListWithC2 c sss where
	zipListWithC2 :: (forall s . c s => t s -> t' s -> a) ->
		PL2 t sss -> PL2 t' sss -> [[a]]

instance ZipListWithC2 c '[] where zipListWithC2 _ Nil Nil = []

instance (ZipListWithC c ss, ZipListWithC2 c sss) =>
	ZipListWithC2 c (ss ': sss) where
	zipListWithC2 f (xs :** xss) (ys :** yss) =
		zipListWithC @c f xs ys : zipListWithC2 @c f xss yss

class ZipListWithC3 c ssss where
	zipListWithC3 :: (forall s . c s => t s -> t' s -> a) ->
		PL3 t ssss -> PL3 t' ssss -> [[[a]]]

instance ZipListWithC3 c '[] where zipListWithC3 _ Nil Nil = []

instance (ZipListWithC2 c sss, ZipListWithC3 c ssss) =>
	ZipListWithC3 c (sss ': ssss) where
	zipListWithC3 f (xss :** xsss) (yss :** ysss) =
		zipListWithC2 @c f xss yss : zipListWithC3 @c f xsss ysss

class ToListWithCM c ss where
	toListWithCM :: Applicative m =>
		(forall s . c s => t s -> m a) -> PL t ss -> m [a]

instance ToListWithCM c '[] where toListWithCM _ Nil = pure []

instance (c s, ToListWithCM c ss) => ToListWithCM c (s ': ss) where
	toListWithCM f (x :** xs) = (:) <$> f x <*> toListWithCM @c f xs

class ToListWithCM' c (t' :: k -> k') (ss :: [k]) where
	toListWithCM' :: Applicative m =>
		(forall (s :: k) . c (t' s) => t s -> m a) -> PL t ss -> m [a]

instance ToListWithCM' c t' '[] where toListWithCM' _ Nil = pure []

instance (c (t' s), ToListWithCM' c t' ss) =>
	ToListWithCM' c t' (s ': ss) where
	toListWithCM' f (x :** xs) =
		(:) <$> f x <*> toListWithCM' @_ @_ @c @t' f xs

class ToListWithCCpsM c ns where
	toListWithCCpsM ::
		(forall s . c s => t s -> (a -> m b) -> m b) -> PL t ns ->
		([a] -> m b) -> m b

instance ToListWithCCpsM c '[] where toListWithCCpsM _ Nil g = g []

instance (c n, ToListWithCCpsM c ns) =>
	ToListWithCCpsM c (n ': ns) where
	toListWithCCpsM f (x :** xs) g =
		f x \y -> toListWithCCpsM @c f xs \ys -> g $ y : ys

class ToListWithCCpsM' c (t' :: k -> k') (ns :: [k]) where
	toListWithCCpsM' ::
		(forall (s :: k) . c (t' s) => t s -> (a -> m b) -> m b) ->
		PL t ns -> ([a] -> m b) -> m b

instance ToListWithCCpsM' c t' '[] where toListWithCCpsM' _ Nil = ($ [])

instance (c (t' n), ToListWithCCpsM' c t' ns) =>
	ToListWithCCpsM' c t' (n ': ns) where
	toListWithCCpsM' f (x :** xs) g =
		f x \y -> toListWithCCpsM' @_ @_ @c @t' f xs \ys -> g $ y : ys

withListWithCCpsM' :: forall k c t' ns t m a b .
	ToListWithCCpsM' c t' ns =>
	PL t ns ->
	(forall (s :: k) . c (t' s) => t s -> (a -> m b) -> m b) ->
	([a] -> m b) -> m b
withListWithCCpsM' xs f = toListWithCCpsM' @_ @_ @c @t' f xs
