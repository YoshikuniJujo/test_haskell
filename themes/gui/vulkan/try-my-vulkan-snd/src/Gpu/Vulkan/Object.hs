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

module Gpu.Vulkan.Object (

	-- * OBJECT

	Object(..),

	-- ** Synonyms

	StObj, DynObj,

	-- *** static

	Atom, List, Image,

	-- *** dynamic

	DynAtom, DynList, DynImage,

	-- ** Type Of Object

	TypeOfObject,

	-- * OBJECT LENGTH

	ObjectLength,

	-- ** Synonyms

	pattern ObjectLengthAtom,
	pattern ObjectLengthList,
	pattern ObjectLengthImage,
	pattern ObjectLengthDynAtom,
	pattern ObjectLengthDynList,
	pattern ObjectLengthDynImage,

	-- ** Find Length

	ObjectLengthOf(..), ObjectLengthForTypeName(..),

	-- * ONLY DYNAMIC LENGTHS

	OnlyDynamicLengths(..),

	-- * STORE

	StoreObject(..),

	-- * SIZE, ALIGNMENT AND OFFSET

	-- ** Whole Size

	wholeSizeNew,

	-- ** Offset Range

	offsetRange, offsetSize, OffsetRange,

	-- ** Offset Of List

	offsetOfList, OffsetOfList(..),

	-- ** Size AlignmentList

	SizeAlignmentList(..), SizeAlignment(..),

	) where

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

-- OBJECT

data Object = Static K.Object | Dynamic Nat K.Object

type StObj algn mnm ot v = Static ('K.Object algn mnm ot v)
type DynObj n algn mnm ot v = Dynamic n ('K.Object algn mnm ot v)

type Atom algn v mnm = Static (K.Atom algn v mnm)
type List algn v nm = Static (K.List algn v nm)
type Image algn v nm = Static (K.Image algn v nm)

type DynAtom n algn v nm = Dynamic n (K.Atom algn v nm)
type DynList n algn v nm = Dynamic n (K.List algn v nm)
type DynImage n algn v nm = Dynamic n (K.Image algn v nm)

type family TypeOfObject obj where
	TypeOfObject (Static kobj) = K.TypeOfObject kobj
	TypeOfObject (Dynamic n kobj) = K.TypeOfObject kobj

-- OBJECT LENGTH

data ObjectLength obj where
	ObjectLengthStatic :: K.ObjectLength kobj -> ObjectLength ('Static kobj)
	ObjectLengthDynamic ::
		K.ObjectLength kobj -> ObjectLength ('Dynamic n kobj)

deriving instance Eq (ObjectLength obj)
deriving instance Show (ObjectLength obj)

pattern ObjectLengthAtom :: ObjectLength ('Static (K.Atom algn v nm))
pattern ObjectLengthAtom <- ObjectLengthStatic K.ObjectLengthAtom where
	ObjectLengthAtom = ObjectLengthStatic K.ObjectLengthAtom

{-# COMPLETE ObjectLengthList #-}

pattern ObjectLengthList :: Device.M.Size -> ObjectLength ('Static (K.List algn v nm))
pattern ObjectLengthList n <- ObjectLengthStatic (K.ObjectLengthList n) where
	ObjectLengthList n = ObjectLengthStatic (K.ObjectLengthList n)

{-# COMPLETE ObjectLengthImage #-}

pattern ObjectLengthImage ::
	Device.M.Size -> Device.M.Size -> Device.M.Size -> Device.M.Size -> ObjectLength ('Static (K.Image algn v nm))
pattern ObjectLengthImage kr kw kh kd <- (ObjectLengthStatic (K.ObjectLengthImage kr kw kh kd))
	where ObjectLengthImage kr kw kh kd = ObjectLengthStatic (K.ObjectLengthImage kr kw kh kd)

pattern ObjectLengthDynAtom :: ObjectLength ('Dynamic n (K.Atom algn v nm))
pattern ObjectLengthDynAtom <- ObjectLengthDynamic K.ObjectLengthAtom where
	ObjectLengthDynAtom = ObjectLengthDynamic K.ObjectLengthAtom

pattern ObjectLengthDynList :: Device.M.Size -> ObjectLength ('Dynamic n (K.List algn v nm))
pattern ObjectLengthDynList n <- ObjectLengthDynamic (K.ObjectLengthList n) where
	ObjectLengthDynList n = ObjectLengthDynamic (K.ObjectLengthList n)

pattern ObjectLengthDynImage ::
	Device.M.Size -> Device.M.Size -> Device.M.Size -> Device.M.Size -> ObjectLength ('Dynamic n (K.Image algn v nm))
pattern ObjectLengthDynImage kr kw kh kd <- (ObjectLengthDynamic (K.ObjectLengthImage kr kw kh kd))
	where ObjectLengthDynImage kr kw kh kd = ObjectLengthDynamic (K.ObjectLengthImage kr kw kh kd)

class ObjectLengthOf (obj :: Object) (objs :: [Object]) where
	objectLengthOf :: HeteroParList.PL ObjectLength objs -> ObjectLength obj

instance ObjectLengthOf obj (obj ': objs) where
	objectLengthOf (ln :** _) = ln

instance {-# OVERLAPPABLE #-} ObjectLengthOf obj objs =>
	ObjectLengthOf obj (obj' ': objs) where
	objectLengthOf (_ :** lns) = objectLengthOf @obj lns

class ObjectLengthForTypeName v nm objs where
	objectLengthForTypeName ::
		HeteroParList.PL ObjectLength objs -> (
			forall algn . KnownNat algn =>
			ObjectLength (List algn v nm) -> a) -> a

instance KnownNat algn =>
	ObjectLengthForTypeName v nm (List algn v nm ': _objs) where
	objectLengthForTypeName (ln :** _) = ($ ln)

instance {-# OVERLAPPABLE #-}
	ObjectLengthForTypeName v nm objs =>
	ObjectLengthForTypeName v nm (_obj ': objs) where
	objectLengthForTypeName (_ :** lns) f =
		objectLengthForTypeName @v @nm @objs lns f

-- ONLY DYNAMIC LENGTH

class OnlyDynamicLengths (objs :: [Object]) where
	type OnlyDynamics objs :: [K.Object]
	onlyDynamicLength ::
		HeteroParList.PL ObjectLength objs ->
		HeteroParList.PL K.ObjectLength (OnlyDynamics objs)

instance OnlyDynamicLengths '[] where
	type OnlyDynamics '[] = '[]
	onlyDynamicLength HeteroParList.Nil = HeteroParList.Nil

instance OnlyDynamicLengths objs => OnlyDynamicLengths ('Static _o ': objs) where
	type OnlyDynamics ('Static _o ': objs) = OnlyDynamics objs
	onlyDynamicLength (ObjectLengthStatic _ :** lns) = onlyDynamicLength lns

instance OnlyDynamicLengths objs => OnlyDynamicLengths ('Dynamic _n o ': objs) where
	type OnlyDynamics ('Dynamic _n o ': objs) = o ': OnlyDynamics objs
	onlyDynamicLength (ObjectLengthDynamic ln :** lns) =
		ln :** onlyDynamicLength lns

-- STORE

class StoreObject v obj where
	storeObject :: Ptr (TypeOfObject obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (TypeOfObject obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

instance K.StoreObject v bobj => StoreObject v (Static bobj) where
	storeObject p (ObjectLengthStatic kln) = K.storeObject p kln
	loadObject p (ObjectLengthStatic kln) = K.loadObject p kln
	objectLength = ObjectLengthStatic . K.objectLength

instance (KnownNat n, K.StoreObject v bobj) =>
	StoreObject [Maybe v] (Dynamic n bobj) where
	storeObject p0 (ObjectLengthDynamic kln) =
		go p0 (natVal (Proxy :: Proxy n))
		where
		go _ _ [] = pure ()
		go _ n _ | n < 1 = pure ()
		go p n (Just x : xs) = do
			K.storeObject p kln x >> go (nextObject p kln) (n - 1) xs
		go p n (Nothing : xs) = do
			go (nextObject p kln) (n - 1) xs
	loadObject p0 (ObjectLengthDynamic kln) = go p0 (natVal (Proxy :: Proxy n))
		where
		go _ n | n < 1 = pure []
		go p n = (:) <$> (Just <$> K.loadObject p kln) <*> go (nextObject p kln) (n - 1)
	objectLength = ObjectLengthDynamic . K.objectLength . fromJust . head

nextObject :: forall kobj . K.SizeAlignment kobj =>
	Ptr (K.TypeOfObject kobj) -> K.ObjectLength kobj -> Ptr (K.TypeOfObject kobj)
nextObject p ln = p `plusPtr` fromIntegral n
	where
	n = ((K.objectSize ln - 1) `div` algn + 1) * algn
	algn = K.objectAlignment @kobj

-- SIZE, ALIGNMENT AND OFFSET

-- WholeSize

wholeSizeNew :: SizeAlignmentList objs =>
	HeteroParList.PL ObjectLength objs -> Size
wholeSizeNew = wholeSizeFromSizeAlignmentList 0 . sizeAlignmentList

wholeSizeFromSizeAlignmentList ::
	Size -> HeteroParList.PL SizeAlignmentOfObj objs -> Size
wholeSizeFromSizeAlignmentList sz0 HeteroParList.Nil = sz0
wholeSizeFromSizeAlignmentList sz0 (SizeAlignmentOfObj dn sz algn :** saoo) =
	wholeSizeFromSizeAlignmentList
		(((sz0 - 1) `div` algn + 1) * algn + dn * sz) saoo

-- Offset

offsetRange :: forall obj objs . OffsetRange obj objs => Device.M.Size ->
	HeteroParList.PL ObjectLength objs -> (Device.M.Size, Device.M.Size)
offsetRange ost0 lns =
	offsetRangeFromSizeAlignmentList @obj (fromIntegral ost0)
		$ sizeAlignmentList lns

offsetSize :: forall obj objs . OffsetRange obj objs => Device.M.Size ->
	HeteroParList.PL ObjectLength objs -> (Device.M.Size, Device.M.Size)
offsetSize ost0 lns =
	offsetSizeFromSizeAlignmentList @obj (fromIntegral ost0)
		$ sizeAlignmentList lns

class (SizeAlignmentList vs, HeteroParList.TypeIndex v vs) =>
	OffsetRange (v :: Object) (vs :: [Object]) where
	offsetRangeFromSizeAlignmentList ::
		Device.M.Size -> HeteroParList.PL SizeAlignmentOfObj vs ->
		(Device.M.Size, Device.M.Size)
	offsetSizeFromSizeAlignmentList ::
		Device.M.Size -> HeteroParList.PL SizeAlignmentOfObj vs ->
		(Device.M.Size, Device.M.Size)

instance (SizeAlignment v, SizeAlignmentList vs) =>
	OffsetRange v (v ': vs) where
	offsetRangeFromSizeAlignmentList ost (SizeAlignmentOfObj _dn sz algn :** _) =
		(adjust algn ost, fromIntegral sz)
	offsetSizeFromSizeAlignmentList ost (SizeAlignmentOfObj dn sz algn :** _) =
		(adjust algn ost, fromIntegral $ dn * sz)

instance {-# OVERLAPPABLE #-} (SizeAlignment v', OffsetRange v vs) =>
	OffsetRange v (v' ': vs) where
	offsetRangeFromSizeAlignmentList ost (SizeAlignmentOfObj dn sz algn :** sas) =
		offsetRangeFromSizeAlignmentList @v @vs (adjust algn ost + dn * sz) sas
	offsetSizeFromSizeAlignmentList ost (SizeAlignmentOfObj dn sz algn :** sas) =
		offsetSizeFromSizeAlignmentList @v @vs (adjust algn ost + dn * sz) sas

-- OffsetOfList

offsetOfList :: forall v onm vs . OffsetOfList v onm vs =>
	HeteroParList.PL ObjectLength vs -> Device.M.Size
offsetOfList = offsetListFromSizeAlignmentList @v @onm 0 . sizeAlignmentList

class SizeAlignmentList objs =>
	OffsetOfList v (nm :: Symbol) (objs :: [Object]) where
	offsetListFromSizeAlignmentList ::
		Device.M.Size -> HeteroParList.PL SizeAlignmentOfObj objs ->
		Device.M.Size

instance (Storable v, KnownNat oalgn, SizeAlignmentList objs) =>
	OffsetOfList v nm (List oalgn v nm ': objs) where
	offsetListFromSizeAlignmentList
		ost (SizeAlignmentOfObj _ _ algn :** _) = adjust algn ost

instance {-# OVERLAPPABLE #-} (SizeAlignment obj, OffsetOfList v nm objs) =>
	OffsetOfList v nm (obj ': objs) where
	offsetListFromSizeAlignmentList
		ost (SizeAlignmentOfObj dn sz algn :** sas) =
		offsetListFromSizeAlignmentList
			@v @nm @objs (adjust algn ost + dn * sz) sas

adjust :: Device.M.Size -> Device.M.Size -> Device.M.Size
adjust algn ost = ((ost - 1) `div` algn + 1) * algn

-- SizeAlignmentList

data SizeAlignmentOfObj (obj :: Object) =
	SizeAlignmentOfObj DynNum Size ObjAlignment deriving Show

type DynNum = Device.M.Size
type Size = Device.M.Size
type ObjAlignment = Device.M.Size

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

-- SizeAlignment

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Device.M.Size
	dynNum :: Device.M.Size
	objectSize' :: ObjectLength obj -> Device.M.Size
	objectAlignment :: Device.M.Size

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
