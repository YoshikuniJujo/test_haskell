{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Object (

	-- * OBJECT

	O(..),

	-- ** Synonyms

	Static, Dynamic,

	-- *** static

	Atom, List, Image,
	AtomNoName, ListNoName, ImageNoName,
	AtomMaybeName, ListMaybeName, ImageMaybeName,

	-- *** dynamic

	DynAtomNew, DynList, DynImage,
	DynAtomNoName, DynListNoName, DynImageNoName,
	DynAtomMaybeName, DynListMaybeName, DynImageMaybeName,

	-- ** Type Of Object

	TypeOf,

	-- * OBJECT LENGTH

	Length,

	-- ** Synonyms

	pattern LengthAtom,
	pattern LengthList,
	pattern LengthImage,
	pattern LengthDynAtom,
	pattern LengthDynList,
	pattern LengthDynImage,

	pattern LengthList',

	-- ** Find Length

	LengthOf(..),

	-- * ONLY DYNAMIC LENGTHS

	OnlyDynamicLengths(..),

	-- * STORE

	Store(..),

	-- * SIZE, ALIGNMENT AND OFFSET

	-- ** Whole Size

	wholeSize, WholeAlign(..),

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
import Data.Word

import Foreign.Storable (Storable)
import Gpu.Vulkan.Object.Base qualified as K
import Gpu.Vulkan.Device.Middle.Internal qualified as Device.M

-- OBJECT

data O = Static_ K.O | Dynamic Nat K.O

type Static algn mnm ot v = 'Static_ ('K.O algn mnm ot v)
type Dynamic n algn mnm ot v = 'Dynamic n ('K.O algn mnm ot v)

type Atom algn v nm = AtomMaybeName algn v ('Just nm)
type List algn v nm = ListMaybeName algn v ('Just nm)
type Image algn v nm = ImageMaybeName algn v ('Just nm)

type AtomNoName algn v = AtomMaybeName algn v 'Nothing
type ListNoName algn v = ListMaybeName algn v 'Nothing
type ImageNoName algn v = ImageMaybeName algn v 'Nothing

type AtomMaybeName algn v mnm = Static_ (K.AtomMaybeName algn v mnm)
type ListMaybeName algn v mnm = Static_ (K.ListMaybeName algn v mnm)
type ImageMaybeName algn v mnm = Static_ (K.ImageMaybeName algn v mnm)

type DynAtomNew n algn v nm = 'Dynamic n (K.AtomNew algn v nm)
type DynList n algn v nm = 'Dynamic n (K.List algn v nm)
type DynImage n algn v nm = 'Dynamic n (K.Image algn v nm)

type DynAtomNoName n algn v = 'Dynamic n (K.AtomNoName algn v)
type DynListNoName n algn v = 'Dynamic n (K.ListNoName algn v)
type DynImageNoName n algn v = 'Dynamic n (K.ImageNoName algn v)

type DynAtomMaybeName n algn v mnm = 'Dynamic n (K.AtomMaybeName algn v mnm)
type DynListMaybeName n algn v mnm = 'Dynamic n (K.ListMaybeName algn v mnm)
type DynImageMaybeName n algn v mnm = 'Dynamic n (K.ImageMaybeName algn v mnm)

type family TypeOf obj where
	TypeOf (Static_ kobj) = K.TypeOf kobj
	TypeOf ('Dynamic n kobj) = K.TypeOf kobj

-- OBJECT LENGTH

data Length obj where
	LengthStatic :: K.Length kobj -> Length ('Static_ kobj)
	LengthDynamic ::
		K.Length kobj -> Length ('Dynamic n kobj)

deriving instance Eq (Length obj)
deriving instance Show (Length obj)

pattern LengthAtom :: Length ('Static_ (K.AtomMaybeName algn v mnm))
pattern LengthAtom <- LengthStatic K.LengthAtom where
	LengthAtom = LengthStatic K.LengthAtom

{-# COMPLETE LengthList #-}
{-# COMPLETE LengthList' #-}

pattern LengthList :: Device.M.Size ->
	Length ('Static_ (K.ListMaybeName algn v mnm))
pattern LengthList n <- LengthStatic (K.LengthList n) where
	LengthList n = LengthStatic (K.LengthList n)

pattern LengthList' :: Word64 -> Length ('Static_ (K.ListMaybeName algn v nm))
pattern LengthList' n <- LengthStatic (K.LengthList (Device.M.Size n)) where
	LengthList' n = LengthStatic (K.LengthList (Device.M.Size n))

{-# COMPLETE LengthImage #-}

pattern LengthImage ::
	Device.M.Size -> Device.M.Size -> Device.M.Size -> Device.M.Size ->
	Length ('Static_ (K.ImageMaybeName algn v mnm))
pattern LengthImage kr kw kh kd <- (LengthStatic (K.LengthImage kr kw kh kd))
	where LengthImage kr kw kh kd = LengthStatic (K.LengthImage kr kw kh kd)

pattern LengthDynAtom :: Length ('Dynamic n (K.Atom algn v nm))
pattern LengthDynAtom <- LengthDynamic K.LengthAtom where
	LengthDynAtom = LengthDynamic K.LengthAtom

pattern LengthDynList :: Device.M.Size -> Length ('Dynamic n (K.List algn v nm))
pattern LengthDynList n <- LengthDynamic (K.LengthList n) where
	LengthDynList n = LengthDynamic (K.LengthList n)

pattern LengthDynImage ::
	Device.M.Size -> Device.M.Size -> Device.M.Size -> Device.M.Size -> Length ('Dynamic n (K.Image algn v nm))
pattern LengthDynImage kr kw kh kd <- (LengthDynamic (K.LengthImage kr kw kh kd))
	where LengthDynImage kr kw kh kd = LengthDynamic (K.LengthImage kr kw kh kd)

class LengthOf (obj :: O) (objs :: [O]) where
	lengthOf :: HeteroParList.PL Length objs -> Length obj

instance LengthOf obj (obj ': objs) where lengthOf (ln :** _) = ln

instance {-# OVERLAPPABLE #-} LengthOf obj objs =>
	LengthOf obj (obj' ': objs) where
	lengthOf (_ :** lns) = lengthOf @obj lns

-- ONLY DYNAMIC LENGTH

class OnlyDynamicLengths (objs :: [O]) where
	type OnlyDynamics objs :: [K.O]
	onlyDynamicLength ::
		HeteroParList.PL Length objs ->
		HeteroParList.PL K.Length (OnlyDynamics objs)

instance OnlyDynamicLengths '[] where
	type OnlyDynamics '[] = '[]
	onlyDynamicLength HeteroParList.Nil = HeteroParList.Nil

instance OnlyDynamicLengths objs => OnlyDynamicLengths ('Static_ _o ': objs) where
	type OnlyDynamics ('Static_ _o ': objs) = OnlyDynamics objs
	onlyDynamicLength (LengthStatic _ :** lns) = onlyDynamicLength lns

instance OnlyDynamicLengths objs => OnlyDynamicLengths ('Dynamic _n o ': objs) where
	type OnlyDynamics ('Dynamic _n o ': objs) = o ': OnlyDynamics objs
	onlyDynamicLength (LengthDynamic ln :** lns) =
		ln :** onlyDynamicLength lns

-- STORE

class Store v obj where
	store :: Ptr (TypeOf obj) -> Length obj -> v -> IO ()
	load :: Ptr (TypeOf obj) -> Length obj -> IO v
	length :: v -> Length obj

instance K.Store v bobj => Store v (Static_ bobj) where
	store p (LengthStatic kln) = K.store p kln
	load p (LengthStatic kln) = K.load p kln
	length = LengthStatic . K.length

instance (KnownNat n, K.Store v bobj) => Store [Maybe v] ('Dynamic n bobj) where
	store p0 (LengthDynamic kln) =
		go p0 (natVal (Proxy :: Proxy n))
		where
		go _ _ [] = pure ()
		go _ n _ | n < 1 = pure ()
		go p n (Just x : xs) = do
			K.store p kln x >> go (nextObject p kln) (n - 1) xs
		go p n (Nothing : xs) = do
			go (nextObject p kln) (n - 1) xs
	load p0 (LengthDynamic kln) = go p0 (natVal (Proxy :: Proxy n))
		where
		go _ n | n < 1 = pure []
		go p n = (:) <$> (Just <$> K.load p kln) <*> go (nextObject p kln) (n - 1)
	length = LengthDynamic . K.length . fromJust . head

nextObject :: forall kobj . K.SizeAlignment kobj =>
	Ptr (K.TypeOf kobj) -> K.Length kobj -> Ptr (K.TypeOf kobj)
nextObject p ln = p `plusPtr` fromIntegral n
	where
	n = ((K.size ln - 1) `div` algn + 1) * algn
	algn = K.alignment @kobj

-- SIZE, ALIGNMENT AND OFFSET

-- WholeSize

wholeSize :: SizeAlignmentList objs =>
	HeteroParList.PL Length objs -> Size
wholeSize = wholeSizeFromSzAlgns 0 . sizeAlignmentList

wholeSizeFromSzAlgns ::
	Size -> HeteroParList.PL SizeAlignmentOf objs -> Size
wholeSizeFromSzAlgns sz0 HeteroParList.Nil = sz0
wholeSizeFromSzAlgns sz0 (SizeAlignmentOf dn sz algn :** saoo) =
	wholeSizeFromSzAlgns (adjust algn sz0 + dn * sz) saoo

class WholeAlign (objs :: [O]) where wholeAlign :: Size

instance WholeAlign '[] where wholeAlign = 1

instance (SizeAlignment obj, WholeAlign objs) =>
	WholeAlign (obj ': objs) where
	wholeAlign = alignment @obj `lcm` wholeAlign @objs

-- OffsetRange

offsetRange :: forall obj objs i . OffsetRange obj objs i => Device.M.Size ->
	HeteroParList.PL Length objs -> (Device.M.Size, Device.M.Size)
offsetRange ost0 lns = offsetRangeFromSzAlgns' @obj @_ @i ost0 $ sizeAlignmentList lns

offsetSize :: forall obj objs i . OffsetRange obj objs i => Device.M.Size ->
	HeteroParList.PL Length objs -> (Device.M.Size, Device.M.Size)
offsetSize ost0 lns = offsetSizeFromSzAlgns' @obj @_ @i ost0 $ sizeAlignmentList lns

class (SizeAlignmentList vs, HeteroParList.TypeIndex v vs) =>
	OffsetRange (v :: O) (vs :: [O]) (i :: Nat) where
	offsetRangeFromSzAlgns' ::
		Device.M.Size -> HeteroParList.PL SizeAlignmentOf vs ->
		(Device.M.Size, Device.M.Size)
	offsetSizeFromSzAlgns' ::
		Device.M.Size -> HeteroParList.PL SizeAlignmentOf vs ->
		(Device.M.Size, Device.M.Size)

instance (SizeAlignment v, SizeAlignmentList vs) =>
	OffsetRange v (v ': vs) 0 where
	offsetRangeFromSzAlgns' ost (SizeAlignmentOf _dn sz algn :** _) =
		(adjust algn ost, sz)
	offsetSizeFromSzAlgns' ost (SizeAlignmentOf dn sz algn :** _) =
		(adjust algn ost, dn * sz)

instance {-# OVERLAPPABLE #-}
	(SizeAlignment v, OffsetRange v vs (i - 1)) =>
	OffsetRange v (v ': vs) i where
	offsetRangeFromSzAlgns' ost (SizeAlignmentOf dn sz algn :** sas) =
		offsetRangeFromSzAlgns' @v @vs @(i - 1) (adjust algn ost + dn * sz) sas
	offsetSizeFromSzAlgns' ost (SizeAlignmentOf dn sz algn :** sas) =
		offsetSizeFromSzAlgns' @v @vs @(i - 1) (adjust algn ost + dn * sz) sas

instance {-# OVERLAPPABLE #-} (SizeAlignment v', OffsetRange v vs i) =>
	OffsetRange v (v' ': vs) i where
	offsetRangeFromSzAlgns' ost (SizeAlignmentOf dn sz algn :** sas) =
		offsetRangeFromSzAlgns' @v @vs @i (adjust algn ost + dn * sz) sas
	offsetSizeFromSzAlgns' ost (SizeAlignmentOf dn sz algn :** sas) =
		offsetSizeFromSzAlgns' @v @vs @i (adjust algn ost + dn * sz) sas

-- OffsetOfList

offsetOfList :: forall v onm vs . OffsetOfList v onm vs =>
	HeteroParList.PL Length vs -> (Device.M.Size, Device.M.Size)
offsetOfList = offsetRangeListFromSzAlgns @v @onm 0 . sizeAlignmentList

class SizeAlignmentList objs =>
	OffsetOfList v (nm :: Symbol) (objs :: [O]) where
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

data SizeAlignmentOf (obj :: O) = SizeAlignmentOf DynNum Size ObjAlignment
	deriving Show

type DynNum = Device.M.Size
type Size = Device.M.Size
type ObjAlignment = Device.M.Size

class SizeAlignmentList objs where
	sizeAlignmentList :: HeteroParList.PL Length objs ->
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
	size :: Length obj -> Device.M.Size
	alignment :: Device.M.Size

instance K.SizeAlignment kobj => SizeAlignment (Static_ kobj) where
	dynNum = 1
	size (LengthStatic kln) = K.size kln
	alignment = K.alignment @kobj

instance (KnownNat n, K.SizeAlignment kobj) =>
	SizeAlignment ('Dynamic n kobj) where
	dynNum = fromIntegral $ natVal (Proxy :: Proxy n)
	size (LengthDynamic kln) = K.size kln
	alignment = K.alignment @kobj
