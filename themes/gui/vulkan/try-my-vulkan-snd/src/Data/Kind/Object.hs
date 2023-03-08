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

	SizeAlignment(..), WholeSize(..),

	StoreObject(..), Offset(..),

	IsImage(..), Atom, List,
	pattern ObjectLengthAtom, pattern ObjectLengthList
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
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Data.Sequences as Seq

import Data.Kind.ObjectNew qualified as N

data Object = ObjObject N.Object | ObjImage N.Alignment Type Symbol

type Atom algn t mnm = 'ObjObject ('N.Atom algn t mnm)
type List algn t nm = 'ObjObject ('N.List algn t nm)

pattern ObjectLengthAtom :: ObjectLength (ObjObject ('N.Atom _algn t nm))
pattern ObjectLengthAtom <- ObjectLengthObject N.ObjectLengthAtom where
	ObjectLengthAtom = ObjectLengthObject N.ObjectLengthAtom

pattern ObjectLengthList :: Int -> ObjectLength (ObjObject ('N.List _algn t nm))
pattern ObjectLengthList n <- ObjectLengthObject (N.ObjectLengthList n) where
	ObjectLengthList n = ObjectLengthObject $ N.ObjectLengthList n

type family ObjectType obj where
	ObjectType ('ObjObject ('N.Atom _algn t _nm)) = t
	ObjectType ('ObjObject ('N.List _algn t _nm)) = t
	ObjectType ('ObjImage _algn t _nm) = t

data ObjectLength (obj :: Object) where
	ObjectLengthObject :: N.ObjectLength obj -> ObjectLength (ObjObject obj)
	ObjectLengthImage :: {
		objectLengthImageRow :: Int,
		objectLengthImageWidth :: Int,
		objectLengthImageHeight :: Int,
		objectLengthImageDepth :: Int } -> ObjectLength ('ObjImage algn t nm)

deriving instance Eq (ObjectLength obj)

deriving instance Show (ObjectLength obj)

class Offset (obj :: Object) objs where
	offset :: Int -> HeteroParList.PL ObjectLength objs -> Int
	range :: HeteroParList.PL ObjectLength objs -> Int

instance SizeAlignment obj => Offset obj (obj ': objs) where
	offset ofst _ = ((ofst - 1) `div` algn + 1) * algn
		where algn = objectAlignment @obj
	range (ln :** _) = objectSize ln

instance {-# OVERLAPPABLE #-} (SizeAlignment obj', Offset obj objs) =>
	Offset obj (obj' ': objs) where
	offset ofst (ln :** lns) = offset @obj
		(((ofst - 1) `div` algn + 1) * algn + objectSize ln) lns
		where algn = objectAlignment @obj'
	range (_ :** lns) = range @obj lns

class WholeSize objs where
	wholeSize :: Int -> HeteroParList.PL ObjectLength objs -> Int

instance WholeSize '[] where wholeSize sz _ = sz

instance (SizeAlignment obj, WholeSize objs) =>
	WholeSize (obj ': objs) where
	wholeSize sz (ln :** lns) =
		wholeSize (((sz - 1) `div` algn + 1) * algn + objectSize ln) lns
		where algn = objectAlignment @obj

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Int
	objectAlignment :: Int

instance (KnownNat algn, Storable t) =>
	SizeAlignment ('ObjObject ('N.Atom algn t _nm)) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @t undefined
	objectSize (ObjectLengthObject N.ObjectLengthAtom) = sizeOf @t undefined

instance (KnownNat algn, Storable t) =>
	SizeAlignment ('ObjObject ('N.List algn t _nm)) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @t undefined
	objectSize (ObjectLengthObject (N.ObjectLengthList n)) = n * ((sz - 1) `div` algn + 1) * algn
		where
		sz = sizeOf @t undefined
		algn = alignment @t undefined

instance Storable (IsImagePixel img) => SizeAlignment ('ObjImage algn img nm) where
	objectAlignment = alignment @(IsImagePixel img) undefined
	objectSize (ObjectLengthImage r _w h d) =
		r * h * d * ((sz - 1) `div` algn + 1) * algn
		where
		sz = sizeOf @(IsImagePixel img) undefined
		algn = alignment @(IsImagePixel img) undefined

class StoreObject v (obj :: Object) where
	storeObject :: Ptr (ObjectType obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (ObjectType obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

instance Storable t => StoreObject t ('ObjObject ('N.Atom _algn t _nm)) where
	storeObject p (ObjectLengthObject N.ObjectLengthAtom) x = poke p x
	loadObject p (ObjectLengthObject N.ObjectLengthAtom) = peek p
	objectLength _ = ObjectLengthObject N.ObjectLengthAtom

instance (
	MonoFoldable v, Seq.IsSequence v,
	Storable t, Element v ~ t ) =>
	StoreObject v ('ObjObject ('N.List _algn t _nm)) where
	storeObject p (ObjectLengthObject (N.ObjectLengthList n)) xs =
		pokeArray p . take n $ otoList xs
	loadObject p (ObjectLengthObject (N.ObjectLengthList n)) =
		Seq.fromList <$> peekArray n p
	objectLength = ObjectLengthObject . N.ObjectLengthList . olength

instance (IsImage img, Storable (IsImagePixel img)) =>
	StoreObject img ('ObjImage algn img nm) where
	storeObject p0 (ObjectLengthImage r w h d) img =
		for_ (zip (iterate (`plusPtr` s) p0) $ isImageBody img)
			\(p, take w -> rw) -> pokeArray (castPtr p) $ take w rw
		where s = r * sizeOf @(IsImagePixel img) undefined
	loadObject p0 (ObjectLengthImage r w h d) = isImageMake w h d
		<$> for (take (h * d) $ iterate (`plusPtr` s) p0) \p -> peekArray w (castPtr p)
		where s = r * sizeOf @(IsImagePixel img) undefined

class IsImage img where
	type IsImagePixel img
	isImageRow :: img -> Int
	isImageWidth :: img -> Int
	isImageHeight :: img -> Int
	isImageDepth :: img -> Int
	isImageBody :: img -> [[IsImagePixel img]]
	isImageMake :: Int -> Int -> Int -> [[IsImagePixel img]] -> img
