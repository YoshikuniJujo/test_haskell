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

	ObjectLengthOf(..),

	-- * ONLY DYNAMIC LENGTHS

	OnlyDynamicLengths(..),

	-- * STORE

	Store(..),

	-- * SIZE, ALIGNMENT AND OFFSET

	-- ** Whole Size

	wholeSize,

	-- ** Offset Range

	offsetRange, offsetSize, OffsetRange,

	-- ** Offset Of List

	offsetOfList, OffsetOfList(..),

	-- ** Size AlignmentList

	SizeAlignmentList(..), SizeAlignment(..),

	) where

import Prelude hiding (length)

import GHC.TypeLits (Symbol)
import GHC.TypeNats
import Foreign.Ptr
import Data.Proxy
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import Data.Maybe

import Foreign.Storable (Storable)
import Gpu.Vulkan.Object.Base qualified as K
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

class Store v obj where
	store :: Ptr (TypeOfObject obj) -> ObjectLength obj -> v -> IO ()
	load :: Ptr (TypeOfObject obj) -> ObjectLength obj -> IO v
	length :: v -> ObjectLength obj

instance K.StoreObject v bobj => Store v (Static bobj) where
	store p (ObjectLengthStatic kln) = K.storeObject p kln
	load p (ObjectLengthStatic kln) = K.loadObject p kln
	length = ObjectLengthStatic . K.objectLength

instance (KnownNat n, K.StoreObject v bobj) => Store [Maybe v] (Dynamic n bobj) where
	store p0 (ObjectLengthDynamic kln) =
		go p0 (natVal (Proxy :: Proxy n))
		where
		go _ _ [] = pure ()
		go _ n _ | n < 1 = pure ()
		go p n (Just x : xs) = do
			K.storeObject p kln x >> go (nextObject p kln) (n - 1) xs
		go p n (Nothing : xs) = do
			go (nextObject p kln) (n - 1) xs
	load p0 (ObjectLengthDynamic kln) = go p0 (natVal (Proxy :: Proxy n))
		where
		go _ n | n < 1 = pure []
		go p n = (:) <$> (Just <$> K.loadObject p kln) <*> go (nextObject p kln) (n - 1)
	length = ObjectLengthDynamic . K.objectLength . fromJust . head

nextObject :: forall kobj . K.SizeAlignment kobj =>
	Ptr (K.TypeOfObject kobj) -> K.ObjectLength kobj -> Ptr (K.TypeOfObject kobj)
nextObject p ln = p `plusPtr` fromIntegral n
	where
	n = ((K.size ln - 1) `div` algn + 1) * algn
	algn = K.alignment @kobj

-- SIZE, ALIGNMENT AND OFFSET

-- WholeSize

wholeSize :: SizeAlignmentList objs =>
	HeteroParList.PL ObjectLength objs -> Size
wholeSize = wholeSizeFromSzAlgns 0 . sizeAlignmentList

wholeSizeFromSzAlgns ::
	Size -> HeteroParList.PL SizeAlignmentOf objs -> Size
wholeSizeFromSzAlgns sz0 HeteroParList.Nil = sz0
wholeSizeFromSzAlgns sz0 (SizeAlignmentOf dn sz algn :** saoo) =
	wholeSizeFromSzAlgns (adjust algn sz0 + dn * sz) saoo

-- OffsetRange

offsetRange :: forall obj objs . OffsetRange obj objs => Device.M.Size ->
	HeteroParList.PL ObjectLength objs -> (Device.M.Size, Device.M.Size)
offsetRange ost0 lns = offsetRangeFromSzAlgns @obj ost0 $ sizeAlignmentList lns

offsetSize :: forall obj objs . OffsetRange obj objs => Device.M.Size ->
	HeteroParList.PL ObjectLength objs -> (Device.M.Size, Device.M.Size)
offsetSize ost0 lns = offsetSizeFromSzAlgns @obj ost0 $ sizeAlignmentList lns

class (SizeAlignmentList vs, HeteroParList.TypeIndex v vs) =>
	OffsetRange (v :: Object) (vs :: [Object]) where
	offsetRangeFromSzAlgns ::
		Device.M.Size -> HeteroParList.PL SizeAlignmentOf vs ->
		(Device.M.Size, Device.M.Size)
	offsetSizeFromSzAlgns ::
		Device.M.Size -> HeteroParList.PL SizeAlignmentOf vs ->
		(Device.M.Size, Device.M.Size)

instance (SizeAlignment v, SizeAlignmentList vs) =>
	OffsetRange v (v ': vs) where
	offsetRangeFromSzAlgns ost (SizeAlignmentOf _dn sz algn :** _) =
		(adjust algn ost, sz)
	offsetSizeFromSzAlgns ost (SizeAlignmentOf dn sz algn :** _) =
		(adjust algn ost, dn * sz)

instance {-# OVERLAPPABLE #-} (SizeAlignment v', OffsetRange v vs) =>
	OffsetRange v (v' ': vs) where
	offsetRangeFromSzAlgns ost (SizeAlignmentOf dn sz algn :** sas) =
		offsetRangeFromSzAlgns @v @vs (adjust algn ost + dn * sz) sas
	offsetSizeFromSzAlgns ost (SizeAlignmentOf dn sz algn :** sas) =
		offsetSizeFromSzAlgns @v @vs (adjust algn ost + dn * sz) sas

-- OffsetOfList

offsetOfList :: forall v onm vs . OffsetOfList v onm vs =>
	HeteroParList.PL ObjectLength vs -> (Device.M.Size, Device.M.Size)
offsetOfList = offsetRangeListFromSzAlgns @v @onm 0 . sizeAlignmentList

class SizeAlignmentList objs =>
	OffsetOfList v (nm :: Symbol) (objs :: [Object]) where
	offsetRangeListFromSzAlgns ::
		Device.M.Size -> HeteroParList.PL SizeAlignmentOf objs ->
		(Device.M.Size, Device.M.Size)

instance (Storable v, KnownNat oalgn, SizeAlignmentList objs) =>
	OffsetOfList v nm (List oalgn v nm ': objs) where
	offsetRangeListFromSzAlgns ost (SizeAlignmentOf _ sz algn :** _) =
		(adjust algn ost, sz)

instance {-# OVERLAPPABLE #-} (SizeAlignment obj, OffsetOfList v nm objs) =>
	OffsetOfList v nm (obj ': objs) where
	offsetRangeListFromSzAlgns ost (SizeAlignmentOf dn sz algn :** sas) =
		offsetRangeListFromSzAlgns
			@v @nm @objs (adjust algn ost + dn * sz) sas

adjust :: Device.M.Size -> Device.M.Size -> Device.M.Size
adjust algn ost = ((ost - 1) `div` algn + 1) * algn

-- SizeAlignmentList

data SizeAlignmentOf (obj :: Object) = SizeAlignmentOf DynNum Size ObjAlignment
	deriving Show

type DynNum = Device.M.Size
type Size = Device.M.Size
type ObjAlignment = Device.M.Size

class SizeAlignmentList objs where
	sizeAlignmentList :: HeteroParList.PL ObjectLength objs ->
		HeteroParList.PL SizeAlignmentOf objs

instance SizeAlignmentList '[] where
	sizeAlignmentList HeteroParList.Nil = HeteroParList.Nil

instance (SizeAlignment obj, SizeAlignmentList objs) =>
	SizeAlignmentList (obj ': objs) where
	sizeAlignmentList (ln :** lns) =
		SizeAlignmentOf
			(dynNum @obj) (size ln) (alignment @obj) :**
		sizeAlignmentList lns

-- SizeAlignment

class SizeAlignment obj where
	dynNum :: Device.M.Size
	size :: ObjectLength obj -> Device.M.Size
	alignment :: Device.M.Size

instance K.SizeAlignment kobj => SizeAlignment (Static kobj) where
	dynNum = 1
	size (ObjectLengthStatic kln) = K.size kln
	alignment = K.alignment @kobj

instance (KnownNat n, K.SizeAlignment kobj) =>
	SizeAlignment (Dynamic n kobj) where
	dynNum = fromIntegral $ natVal (Proxy :: Proxy n)
	size (ObjectLengthDynamic kln) = K.size kln
	alignment = K.alignment @kobj
