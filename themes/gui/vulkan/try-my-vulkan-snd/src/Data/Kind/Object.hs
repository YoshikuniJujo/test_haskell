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
	Object(..), ObjectLength(..), ObjectType,

	SizeAlignment(..),

	StoreObject(..),

	IsImage(..),
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

import qualified Data.Sequences as Seq

import Gpu.Vulkan.TypeEnum qualified as T

data Object =
	Atom Alignment Type (Maybe Symbol) |
	List Alignment Type Symbol |
	Image Alignment Type Symbol

type Alignment = Nat

data ObjectLength (obj :: Object) where
	ObjectLengthAtom :: ObjectLength ('Atom _algn t nm)
	ObjectLengthList :: Int -> ObjectLength ('List _algn t nm)
	ObjectLengthImage :: {
		objectLengthImageRow :: Int,
		objectLengthImageWidth :: Int,
		objectLengthImageHeight :: Int,
		objectLengthImageDepth :: Int } -> ObjectLength ('Image algn t nm)

deriving instance Eq (ObjectLength obj)
deriving instance Show (ObjectLength obj)

type family ObjectType obj where
	ObjectType (('Atom _algn t _nm)) = t
	ObjectType (('List _algn t _nm)) = t

instance Default (ObjectLength (Atom algn t mnm)) where def = ObjectLengthAtom
instance Default (ObjectLength (List algn t nm)) where def = ObjectLengthList 0
instance Default (ObjectLength (Image algn t nm)) where def = ObjectLengthImage 0 0 0 0

data SizeAlignmentOfObj (obj :: Object) =
	SizeAlignmentOfObj Size ObjAlignment deriving Show

type Size = Int
type ObjAlignment = Int

{-
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
		-}

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Int
	objectAlignment :: Int

applyAlign :: Integral n => n -> n -> n
applyAlign algn ofst = ((ofst - 1) `div` algn + 1) * algn

instance (KnownNat algn, Storable t) =>
	SizeAlignment (('Atom algn t _nm)) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @t undefined
	objectSize (ObjectLengthAtom) = applyAlign algn $ sizeOf @t undefined
		where algn = objectAlignment @(('Atom algn t _nm))

instance (KnownNat algn, Storable t) =>
	SizeAlignment (('List algn t _nm)) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @t undefined
	objectSize ((ObjectLengthList n)) = applyAlign algn' $ n * ((sz - 1) `div` algn + 1) * algn
		where
		sz = sizeOf @t undefined
		algn = alignment @t undefined
		algn' = objectAlignment @(('List algn t _nm))

instance (KnownNat algn, Storable (IsImagePixel img)) => SizeAlignment (('Image algn img nm)) where
	objectAlignment =fromIntegral (natVal (Proxy :: Proxy algn))
		`lcm`
		alignment @(IsImagePixel img) undefined
	objectSize (ObjectLengthImage r _w h d) =
		r * h * d * ((sz - 1) `div` algn + 1) * algn
		where
		sz = sizeOf @(IsImagePixel img) undefined
		algn = objectAlignment @(('Image algn img nm))

class StoreObject v (obj :: Object) where
	storeObject :: Ptr (ObjectType obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (ObjectType obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

instance Storable t => StoreObject t (('Atom _algn t _nm)) where
	storeObject p (ObjectLengthAtom) x = poke p x
	loadObject p (ObjectLengthAtom) = peek p
	objectLength _ = ObjectLengthAtom

instance (
	MonoFoldable v, Seq.IsSequence v,
	Storable t, Element v ~ t ) =>
	StoreObject v (('List _algn t _nm)) where
	storeObject p ((ObjectLengthList n)) xs =
		pokeArray p . take n $ otoList xs
	loadObject p ((ObjectLengthList n)) =
		Seq.fromList <$> peekArray n p
	objectLength = ObjectLengthList . olength

instance (IsImage img, Storable (IsImagePixel img)) =>
	StoreObject img (('Image algn img nm)) where
	storeObject p0 (ObjectLengthImage r w _h _d) img =
		for_ (zip (iterate (`plusPtr` s) p0) $ isImageBody img)
			\(p, take w -> rw) -> pokeArray (castPtr p) $ take w rw
		where s = r * sizeOf @(IsImagePixel img) undefined
	loadObject p0 (ObjectLengthImage r w h d) = isImageMake w h d
		<$> for (take (h * d) $ iterate (`plusPtr` s) p0) \p -> peekArray w (castPtr p)
		where s = r * sizeOf @(IsImagePixel img) undefined
	objectLength img = ObjectLengthImage
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
