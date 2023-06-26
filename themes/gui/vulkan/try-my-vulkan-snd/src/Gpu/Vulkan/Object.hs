{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Object where

import GHC.TypeLits (Symbol)
import GHC.TypeNats
import Foreign.Ptr
import Data.Kind.Object qualified as K
import Data.Proxy
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import Data.Maybe

import Foreign.Storable
import Gpu.Vulkan.Device.Middle qualified as Device.M

data Object = Static K.Object | Dynamic Nat K.Object | Dummy

type List algn t nm = Static (K.List algn t nm)
type Atom algn t mnm = Static (K.Atom algn t mnm)
type ObjImage algn t nm = Static (K.ObjImage algn t nm)

type DynList n algn t nm = Dynamic n (K.List algn t nm)
type DynAtom n algn t nm = Dynamic n (K.Atom algn t nm)

data ObjectLength obj where
	ObjectLengthStatic :: K.ObjectLength kobj -> ObjectLength ('Static kobj)
	ObjectLengthDynamic ::
		K.ObjectLength kobj -> ObjectLength ('Dynamic n kobj)

deriving instance Eq (ObjectLength obj)
deriving instance Show (ObjectLength obj)

{-# COMPLETE ObjectLengthImage #-}

pattern ObjectLengthImage ::
	Int -> Int -> Int -> Int -> ObjectLength ('Static (K.ObjImage algn t nm))
pattern ObjectLengthImage kr kw kh kd <- (ObjectLengthStatic (K.ObjectLengthImage kr kw kh kd))
	where ObjectLengthImage kr kw kh kd = ObjectLengthStatic (K.ObjectLengthImage kr kw kh kd)

pattern ObjectLengthAtom :: ObjectLength ('Static (K.Atom algn t nm))
pattern ObjectLengthAtom <- ObjectLengthStatic K.ObjectLengthAtom where
	ObjectLengthAtom = ObjectLengthStatic K.ObjectLengthAtom

pattern ObjectLengthDynAtom :: ObjectLength ('Dynamic n (K.Atom algn t nm))
pattern ObjectLengthDynAtom <- ObjectLengthDynamic K.ObjectLengthAtom where
	ObjectLengthDynAtom = ObjectLengthDynamic K.ObjectLengthAtom

{-# COMPLETE ObjectLengthList #-}

pattern ObjectLengthList :: Int -> ObjectLength ('Static (K.List algn t nm))
pattern ObjectLengthList n <- ObjectLengthStatic (K.ObjectLengthList n) where
	ObjectLengthList n = ObjectLengthStatic (K.ObjectLengthList n)

pattern ObjectLengthDynList :: Int -> ObjectLength ('Dynamic n (K.List algn t nm))
pattern ObjectLengthDynList n <- ObjectLengthDynamic (K.ObjectLengthList n) where
	ObjectLengthDynList n = ObjectLengthDynamic (K.ObjectLengthList n)

type family ObjectType obj where
	ObjectType (Static kobj) = K.ObjectType kobj
	ObjectType (Dynamic n kobj) = K.ObjectType kobj

wholeSizeNew :: SizeAlignmentList objs =>
	HeteroParList.PL ObjectLength objs -> Size
wholeSizeNew = wholeSizeFromSizeAlignmentList 0 . sizeAlignmentList

wholeSizeFromSizeAlignmentList ::
	Size -> HeteroParList.PL SizeAlignmentOfObj objs -> Size
wholeSizeFromSizeAlignmentList sz0 HeteroParList.Nil = sz0
wholeSizeFromSizeAlignmentList sz0 (SizeAlignmentOfObj dn sz algn :** saoo) =
	wholeSizeFromSizeAlignmentList
		(((sz0 - 1) `div` algn + 1) * algn + dn * sz) saoo

data SizeAlignmentOfObj (obj :: Object) =
	SizeAlignmentOfObj DynNum Size ObjAlignment deriving Show

type DynNum = Int
type Size = Int
type ObjAlignment = Int

class SizeAlignmentList objs where
	sizeAlignmentList :: HeteroParList.PL ObjectLength objs ->
		HeteroParList.PL SizeAlignmentOfObj objs

instance SizeAlignmentList '[] where
	sizeAlignmentList HeteroParList.Nil = HeteroParList.Nil

instance (SizeAlignment obj, SizeAlignmentList objs) =>
	SizeAlignmentList (obj ': objs) where
	sizeAlignmentList (ln :** lns) =
		SizeAlignmentOfObj (dynNum @obj) (objectSize ln) (objectAlignment @obj) :**
		sizeAlignmentList lns

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Int
	dynNum :: Int
	objectSize' :: ObjectLength obj -> Int
	objectAlignment :: Int

instance K.SizeAlignment kobj => SizeAlignment (Static kobj) where
	objectSize (ObjectLengthStatic kln) = K.objectSize kln
	dynNum = 1
	objectSize' = objectSize
	objectAlignment = K.objectAlignment @kobj

instance (KnownNat n, K.SizeAlignment kobj) =>
	SizeAlignment (Dynamic n kobj) where
	objectSize (ObjectLengthDynamic kln) = K.objectSize kln
	dynNum = fromIntegral $ natVal (Proxy :: Proxy n)
	objectSize' obj = fromIntegral (natVal (Proxy :: Proxy n)) * objectSize obj
	objectAlignment = K.objectAlignment @kobj

nextObject :: forall kobj . K.SizeAlignment kobj =>
	Ptr (K.ObjectType kobj) -> K.ObjectLength kobj -> Ptr (K.ObjectType kobj)
nextObject p ln = p `plusPtr` n
	where
	n = ((K.objectSize ln - 1) `div` algn + 1) * algn
	algn = K.objectAlignment @kobj

class StoreObject v obj where
	storeObject :: Ptr (ObjectType obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (ObjectType obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

instance K.StoreObject v kobj => StoreObject v (Static kobj) where
	storeObject p (ObjectLengthStatic kln) = K.storeObject p kln
	loadObject p (ObjectLengthStatic kln) = K.loadObject p kln
	objectLength = ObjectLengthStatic . K.objectLength

instance (K.SizeAlignment kobj, K.StoreObject v kobj, KnownNat n) =>
	StoreObject [Maybe v] (Dynamic n kobj) where
	storeObject p0 (ObjectLengthDynamic kln) =
		go p0 (natVal (Proxy :: Proxy n))
		where
		go _ _ [] = pure ()
		go _ n _ | n < 1 = pure ()
		go p n (Just x : xs) = do
--			putStrLn "Vk.Object.storeObject: go:"
--			print p
			K.storeObject p kln x >> go (nextObject p kln) (n - 1) xs
		go p n (Nothing : xs) = do
--			putStrLn "Vk.Object.storeObject: go:"
--			print p
			go (nextObject p kln) (n - 1) xs
	loadObject p0 (ObjectLengthDynamic kln) = go p0 (natVal (Proxy :: Proxy n))
		where
		go _ n | n < 1 = pure []
		go p n = (:) <$> (Just <$> K.loadObject p kln) <*> go (nextObject p kln) (n - 1)
	objectLength = ObjectLengthDynamic . K.objectLength . fromJust . head

class Offset obj objs => OffsetRange (obj :: Object) objs where
	offset :: Int -> HeteroParList.PL ObjectLength objs -> Int
	range :: HeteroParList.PL ObjectLength objs -> Int

	offset _ = fromIntegral . offsetNew @obj
	range = fromIntegral . rangeNew @obj

instance Offset obj objs => OffsetRange obj objs

class OnlyDynamicLengths (os :: [Object]) where
	type OnlyDynamics os :: [K.Object]
	onlyDynamicLength ::
		HeteroParList.PL ObjectLength os ->
		HeteroParList.PL K.ObjectLength (OnlyDynamics os)

instance OnlyDynamicLengths '[] where
	type OnlyDynamics '[] = '[]
	onlyDynamicLength HeteroParList.Nil = HeteroParList.Nil

instance OnlyDynamicLengths os => OnlyDynamicLengths ('Static _o ': os) where
	type OnlyDynamics ('Static _o ': os) = OnlyDynamics os
	onlyDynamicLength (ObjectLengthStatic _ :** os) = onlyDynamicLength os

instance OnlyDynamicLengths os => OnlyDynamicLengths ('Dynamic _n ko ': os) where
	type OnlyDynamics ('Dynamic _n ko ': os) = ko ': OnlyDynamics os
	onlyDynamicLength (ObjectLengthDynamic kln :** os) =
		kln :** onlyDynamicLength os

instance OnlyDynamicLengths '[ 'Dummy] where
	type OnlyDynamics '[ 'Dummy] = '[]
	onlyDynamicLength _ = HeteroParList.Nil

class ObjectLengthIndex (obj :: k) objs where
	objectLengthIndex ::
		HeteroParList.PL t objs -> t obj

instance ObjectLengthIndex obj (obj ': objs) where
	objectLengthIndex (ln :** _lns) = ln

instance {-# OVERLAPPABLE #-}
	ObjectLengthIndex obj objs =>
	ObjectLengthIndex obj (obj' ': objs) where
	objectLengthIndex (_ :** lns) = objectLengthIndex @_ @obj @objs lns

adjust :: Int -> Int -> Int
adjust algn ost = ((ost - 1) `div` algn + 1) * algn

offsetNew :: forall v vs . Offset v vs =>
	HeteroParList.PL ObjectLength vs -> Device.M.Size
offsetNew = fst . offsetFromSizeAlignmentList @v 0 . sizeAlignmentList

rangeNew :: forall obj objs . Offset obj objs =>
	HeteroParList.PL ObjectLength objs -> Device.M.Size
rangeNew = snd . offsetFromSizeAlignmentList @obj 0 . sizeAlignmentList

class SizeAlignmentList vs => Offset (v :: Object) (vs :: [Object]) where
	offsetFromSizeAlignmentList ::
		Int -> HeteroParList.PL SizeAlignmentOfObj vs ->
		(Device.M.Size, Device.M.Size)

instance (SizeAlignment v, SizeAlignmentList vs) =>
	Offset v (v ': vs) where
	offsetFromSizeAlignmentList ost (SizeAlignmentOfObj dn sz algn :** _) =
		(fromIntegral $ adjust algn ost, fromIntegral sz)

instance {-# OVERLAPPABLE #-} (SizeAlignment v', Offset v vs) =>
	Offset v (v' ': vs) where
	offsetFromSizeAlignmentList ost (SizeAlignmentOfObj dn sz algn :** sas) =
		offsetFromSizeAlignmentList @v @vs (adjust algn ost + dn * sz) sas

class ObjectLengthOf (v :: Object) (vs :: [Object]) where
	objectLengthOf :: HeteroParList.PL ObjectLength vs -> ObjectLength v

instance ObjectLengthOf v (v ': vs) where
	objectLengthOf (ln :** _) = ln

instance {-# OVERLAPPABLE #-} ObjectLengthOf v vs =>
	ObjectLengthOf v (v' ': vs) where
	objectLengthOf (_ :** lns) = objectLengthOf @v lns

class ObjectLengthForTypeName t nm objs where
	objectLengthForTypeName ::
		HeteroParList.PL ObjectLength objs -> (
			forall algn . KnownNat algn =>
			ObjectLength (List algn t nm) -> a) -> a

instance KnownNat algn =>
	ObjectLengthForTypeName t nm (List algn t nm ': _objs) where
	objectLengthForTypeName (ln :** _) = ($ ln)

instance {-# OVERLAPPABLE #-}
	ObjectLengthForTypeName t nm objs =>
	ObjectLengthForTypeName t nm (_obj ': objs) where
	objectLengthForTypeName (_ :** lns) f =
		objectLengthForTypeName @t @nm @objs lns f

offsetOfList :: forall v onm vs . OffsetOfList v onm vs =>
	HeteroParList.PL ObjectLength vs -> Device.M.Size
offsetOfList = offsetListFromSizeAlignmentList @v @onm 0 . sizeAlignmentList

class SizeAlignmentList objs =>
	OffsetOfList t (nm :: Symbol) (objs :: [Object]) where
	offsetListFromSizeAlignmentList ::
		Int -> HeteroParList.PL SizeAlignmentOfObj objs ->
		Device.M.Size

instance (Storable t, KnownNat oalgn, SizeAlignmentList objs) =>
	OffsetOfList t nm (List oalgn t nm ': objs) where
	offsetListFromSizeAlignmentList ost (SizeAlignmentOfObj _ _ algn :** _) =
		fromIntegral $ adjust algn ost

instance {-# OVERLAPPABLE #-}
	(SizeAlignment obj, OffsetOfList t nm objs) =>
	OffsetOfList t nm (obj ': objs) where
	offsetListFromSizeAlignmentList ost (SizeAlignmentOfObj dn sz algn :** sas) =
		offsetListFromSizeAlignmentList @t @nm @objs (adjust algn ost + dn * sz) sas
