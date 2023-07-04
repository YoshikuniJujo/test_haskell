{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Kind.Object (
	Object(..), ObjectLength(..), ObjectType, ObjectAlignment,

	SizeAlignment(..), wholeSizeNew,

	StoreObject(..),

	IsImage(..),
	pattern ObjectLengthAtom, pattern ObjectLengthList, pattern ObjectLengthImageNew
	) where

import GHC.TypeLits
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Kind
import Data.Foldable
import Data.Traversable
import Data.MonoTraversable
import Data.Proxy
import Data.Default
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Data.Sequences as Seq

import Gpu.Vulkan.TypeEnum qualified as T

data Object =
	Atom Alignment Type (Maybe Symbol) |
	List Alignment Type Symbol |
	Image Alignment Type Symbol

type Alignment = Nat

data NObjectLength (obj :: Object) where
	NObjectLengthAtom :: NObjectLength ('Atom _algn t nm)
	NObjectLengthList :: Int -> NObjectLength ('List _algn t nm)
	NObjectLengthImage :: {
		objectLengthImageRow :: Int,
		objectLengthImageWidth :: Int,
		objectLengthImageHeight :: Int,
		objectLengthImageDepth :: Int } -> NObjectLength ('Image algn t nm)

deriving instance Eq (NObjectLength obj)
deriving instance Show (NObjectLength obj)

data ObjectLength obj = ObjectLengthObject (NObjectLength obj)

pattern ObjectLengthAtom :: ObjectLength (('Atom _algn t nm))
pattern ObjectLengthAtom <- ObjectLengthObject NObjectLengthAtom where
	ObjectLengthAtom = ObjectLengthObject NObjectLengthAtom

pattern ObjectLengthList :: Int -> ObjectLength (('List _algn t nm))
pattern ObjectLengthList n <- ObjectLengthObject (NObjectLengthList n) where
	ObjectLengthList n = ObjectLengthObject $ NObjectLengthList n

pattern ObjectLengthImageNew :: Int -> Int -> Int -> Int -> ObjectLength (('Image _algn t nm))
pattern ObjectLengthImageNew r w h d <- ObjectLengthObject (NObjectLengthImage r w h d) where
	ObjectLengthImageNew r w h d = ObjectLengthObject (NObjectLengthImage r w h d)

type family ObjectType obj where
	ObjectType (('Atom _algn t _nm)) = t
	ObjectType (('List _algn t _nm)) = t

instance Default (ObjectLength (Atom algn t mnm)) where def = ObjectLengthAtom
instance Default (ObjectLength (List algn t nm)) where def = ObjectLengthList 0
instance Default (ObjectLength (Image algn t nm)) where def = ObjectLengthImageNew 0 0 0 0

deriving instance Eq (ObjectLength obj)

deriving instance Show (ObjectLength obj)

wholeSizeNew :: SizeAlignmentList objs =>
	HeteroParList.PL ObjectLength objs -> Size
wholeSizeNew = wholeSizeFromSizeAlignmentList 0 . sizeAlignmentList

wholeSizeFromSizeAlignmentList ::
	Size -> HeteroParList.PL SizeAlignmentOfObj objs -> Size
wholeSizeFromSizeAlignmentList sz0 HeteroParList.Nil = sz0
wholeSizeFromSizeAlignmentList sz0 (SizeAlignmentOfObj sz algn :** saoo) =
	wholeSizeFromSizeAlignmentList
		(((sz0 - 1) `div` algn + 1) * algn + sz) saoo

data SizeAlignmentOfObj (obj :: Object) =
	SizeAlignmentOfObj Size ObjAlignment deriving Show

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
		SizeAlignmentOfObj (objectSize ln) (objectAlignment @obj) :**
		sizeAlignmentList lns

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Int
	objectAlignment :: Int

applyAlign :: Integral n => n -> n -> n
applyAlign algn ofst = ((ofst - 1) `div` algn + 1) * algn

instance (KnownNat algn, Storable t) =>
	SizeAlignment (('Atom algn t _nm)) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @t undefined
	objectSize (ObjectLengthObject NObjectLengthAtom) = applyAlign algn $ sizeOf @t undefined
		where algn = objectAlignment @(('Atom algn t _nm))

instance (KnownNat algn, Storable t) =>
	SizeAlignment (('List algn t _nm)) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @t undefined
	objectSize (ObjectLengthObject (NObjectLengthList n)) = applyAlign algn' $ n * ((sz - 1) `div` algn + 1) * algn
		where
		sz = sizeOf @t undefined
		algn = alignment @t undefined
		algn' = objectAlignment @(('List algn t _nm))

instance (KnownNat algn, Storable (IsImagePixel img)) => SizeAlignment (('Image algn img nm)) where
	objectAlignment =fromIntegral (natVal (Proxy :: Proxy algn))
		`lcm`
		alignment @(IsImagePixel img) undefined
	objectSize (ObjectLengthImageNew r _w h d) =
		r * h * d * ((sz - 1) `div` algn + 1) * algn
		where
		sz = sizeOf @(IsImagePixel img) undefined
		algn = objectAlignment @(('Image algn img nm))

class StoreObject v (obj :: Object) where
	storeObject :: Ptr (ObjectType obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (ObjectType obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

instance Storable t => StoreObject t (('Atom _algn t _nm)) where
	storeObject p (ObjectLengthObject NObjectLengthAtom) x = poke p x
	loadObject p (ObjectLengthObject NObjectLengthAtom) = peek p
	objectLength _ = ObjectLengthObject NObjectLengthAtom

instance (
	MonoFoldable v, Seq.IsSequence v,
	Storable t, Element v ~ t ) =>
	StoreObject v (('List _algn t _nm)) where
	storeObject p (ObjectLengthObject (NObjectLengthList n)) xs =
		pokeArray p . take n $ otoList xs
	loadObject p (ObjectLengthObject (NObjectLengthList n)) =
		Seq.fromList <$> peekArray n p
	objectLength = ObjectLengthObject . NObjectLengthList . olength

instance (IsImage img, Storable (IsImagePixel img)) =>
	StoreObject img (('Image algn img nm)) where
	storeObject p0 (ObjectLengthImageNew r w _h _d) img =
		for_ (zip (iterate (`plusPtr` s) p0) $ isImageBody img)
			\(p, take w -> rw) -> pokeArray (castPtr p) $ take w rw
		where s = r * sizeOf @(IsImagePixel img) undefined
	loadObject p0 (ObjectLengthImageNew r w h d) = isImageMake w h d
		<$> for (take (h * d) $ iterate (`plusPtr` s) p0) \p -> peekArray w (castPtr p)
		where s = r * sizeOf @(IsImagePixel img) undefined
	objectLength img = ObjectLengthImageNew
		(isImageRow img) (isImageWidth img) (isImageHeight img) (isImageDepth img)

class IsImage img where
	type IsImagePixel img
	type ImageFormat img :: T.Format
	isImageRow :: img -> Int
	isImageWidth :: img -> Int
	isImageHeight :: img -> Int
	isImageDepth :: img -> Int
	isImageBody :: img -> [[IsImagePixel img]]
	isImageMake :: Int -> Int -> Int -> [[IsImagePixel img]] -> img

type family ObjectAlignment obj where
	ObjectAlignment (Atom algn t nm) = algn
	ObjectAlignment (List algn t nm) = algn
	ObjectAlignment (Image algn t nm) = algn
