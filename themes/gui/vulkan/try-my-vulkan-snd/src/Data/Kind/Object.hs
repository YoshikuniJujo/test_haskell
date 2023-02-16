{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Kind.Object where

import GHC.TypeLits
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Kind
import Data.Foldable
import Data.Traversable
import Data.MonoTraversable
import Data.Proxy
import Data.HeteroParList

import qualified Data.Sequences as Seq

data Object =
	Atom Alignment Type (Maybe Symbol) | List Alignment Type Symbol | ObjImage Type Symbol

type Alignment = Nat

type family ObjectType obj where
	ObjectType ('Atom _algn t _nm) = t
	ObjectType ('List _algn t _nm) = t
	ObjectType ('ObjImage t _nm) = t

data ObjectLength (obj :: Object) where
	ObjectLengthAtom :: ObjectLength ('Atom _algn t nm)
	ObjectLengthList :: Int -> ObjectLength ('List _algn t nm)
	ObjectLengthImage :: {
		objectLengthImageRow :: Int,
		objectLengthImageWidth :: Int,
		objectLengthImageHeight :: Int,
		objectLengthImageDepth :: Int } -> ObjectLength ('ObjImage t nm)

deriving instance Eq (ObjectLength obj)

deriving instance Show (ObjectLength obj)

class Offset (obj :: Object) objs where
	offset :: Int -> HeteroParList ObjectLength objs -> Int
	range :: HeteroParList ObjectLength objs -> Int

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
	wholeSize :: Int -> HeteroParList ObjectLength objs -> Int

instance WholeSize '[] where wholeSize sz _ = sz

minimumAlignment :: Int
-- minimumAlignment = 256
minimumAlignment = 1

instance (SizeAlignment obj, WholeSize objs) =>
	WholeSize (obj ': objs) where
	wholeSize sz (ln :** lns) =
		wholeSize (((sz - 1) `div` algn + 1) * algn + objectSize ln) lns
		where algn = objectAlignment @obj

class StoreObject v (obj :: Object) where
	storeObject :: Ptr (ObjectType obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (ObjectType obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Int
	objectAlignment :: Int

instance Storable t => StoreObject t ('Atom _algn t _nm) where
	storeObject p ObjectLengthAtom x = poke p x
	loadObject p ObjectLengthAtom = peek p
	objectLength _ = ObjectLengthAtom

instance (KnownNat algn, Storable t) => SizeAlignment ('Atom algn t _nm) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @t undefined
	objectSize ObjectLengthAtom = sizeOf @t undefined

instance (
	MonoFoldable v, Seq.IsSequence v,
	Storable t, Element v ~ t ) => StoreObject v ('List _algn t _nm) where
	storeObject p (ObjectLengthList n) xs = pokeArray p . take n $ otoList xs
	loadObject p (ObjectLengthList n) = Seq.fromList <$> peekArray n p
	objectLength = ObjectLengthList . olength

instance (KnownNat algn, Storable t) => SizeAlignment ('List algn t _nm) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		alignment @t undefined
	objectSize (ObjectLengthList n) = n * ((sz - 1) `div` algn + 1) * algn
		where
		sz = sizeOf @t undefined
		algn = alignment @t undefined

class IsImage img where
	type IsImagePixel img
	isImageRow :: img -> Int
	isImageWidth :: img -> Int
	isImageHeight :: img -> Int
	isImageDepth :: img -> Int
	isImageBody :: img -> [[IsImagePixel img]]
	isImageMake :: Int -> Int -> Int -> [[IsImagePixel img]] -> img

instance (IsImage img, Storable (IsImagePixel img)) =>
	StoreObject img ('ObjImage img nm) where
	storeObject p0 (ObjectLengthImage r w h d) img =
		for_ (zip (iterate (`plusPtr` s) p0) $ isImageBody img)
			\(p, take w -> rw) -> pokeArray (castPtr p) $ take w rw
		where s = r * sizeOf @(IsImagePixel img) undefined
	loadObject p0 (ObjectLengthImage r w h d) = isImageMake w h d
		<$> for (take (h * d) $ iterate (`plusPtr` s) p0) \p -> peekArray w (castPtr p)
		where s = r * sizeOf @(IsImagePixel img) undefined

instance Storable (IsImagePixel img) => SizeAlignment ('ObjImage img nm) where
	objectAlignment = alignment @(IsImagePixel img) undefined
	objectSize (ObjectLengthImage r _w h d) =
		r * h * d * ((sz - 1) `div` algn + 1) * algn
		where
		sz = sizeOf @(IsImagePixel img) undefined
		algn = alignment @(IsImagePixel img) undefined
