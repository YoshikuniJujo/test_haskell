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

module Gpu.Vulkan.Object.Base (
	Object(..), ObjectLength(..), TypeOfObject,

	SizeAlignment(..),

	StoreObject(..),

	IsImage(..),

	Atom, List, Image,

	renameObjectLength
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

import Gpu.Vulkan.Device.Middle qualified as Device.M

data Object = Object Alignment (Maybe Symbol) ObjectType Type

data ObjectType = AtomT | ListT | ImageT deriving Show

type Atom algn t mnm = 'Object algn mnm AtomT t
type List algn t nm = 'Object algn ('Just nm) ListT t
type Image algn t nm = 'Object algn ('Just nm) ImageT t

type Alignment = Nat

data ObjectLength (obj :: Object) where
	ObjectLengthAtom :: ObjectLength (Atom algn t nm)
	ObjectLengthList :: Device.M.Size -> ObjectLength ('Object algn mnm ListT t)
	ObjectLengthImage :: {
		objectLengthImageRow :: Device.M.Size,
		objectLengthImageWidth :: Device.M.Size,
		objectLengthImageHeight :: Device.M.Size,
		objectLengthImageDepth :: Device.M.Size } -> ObjectLength ('Object algn mnm ImageT t)

deriving instance Eq (ObjectLength obj)
deriving instance Show (ObjectLength obj)

renameObjectLength :: ObjectLength ('Object algn mnm ot t) ->
	ObjectLength ('Object algn mnm' ot t)
renameObjectLength ObjectLengthAtom = ObjectLengthAtom
renameObjectLength (ObjectLengthList n) = ObjectLengthList n
renameObjectLength (ObjectLengthImage r w h d) = ObjectLengthImage r w h d

type family TypeOfObject obj where
	TypeOfObject ((Atom _algn t _nm)) = t
	TypeOfObject ((List _algn t _nm)) = t

instance Default (ObjectLength (Atom algn t mnm)) where def = ObjectLengthAtom
instance Default (ObjectLength (List algn t nm)) where def = ObjectLengthList 0
instance Default (ObjectLength (Image algn t nm)) where def = ObjectLengthImage 0 0 0 0

data SizeAlignmentOfObj (obj :: Object) =
	SizeAlignmentOfObj Size ObjAlignment deriving Show

type Size = Int
type ObjAlignment = Int

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Device.M.Size
	objectAlignment :: Device.M.Size

applyAlign :: Integral n => n -> n -> n
applyAlign algn ofst = ((ofst - 1) `div` algn + 1) * algn

instance (KnownNat algn, Storable t) =>
	SizeAlignment ((Atom algn t _nm)) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (alignment @t undefined)
	objectSize (ObjectLengthAtom) = applyAlign algn . fromIntegral $ sizeOf @t undefined
		where algn = objectAlignment @((Atom algn t _nm))

instance (KnownNat algn, Storable t) =>
	SizeAlignment ((List algn t _nm)) where
	objectAlignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (alignment @t undefined)
	objectSize (ObjectLengthList n) = applyAlign algn' $ n * ((sz - 1) `div` algn + 1) * algn
		where
		sz = fromIntegral $ sizeOf @t undefined
		algn = fromIntegral $ alignment @t undefined
		algn' = objectAlignment @((List algn t _nm))

instance (KnownNat algn, Storable (IsImagePixel img)) => SizeAlignment ((Image algn img nm)) where
	objectAlignment =fromIntegral (natVal (Proxy :: Proxy algn))
		`lcm`
		fromIntegral (alignment @(IsImagePixel img) undefined)
	objectSize (ObjectLengthImage r _w h d) =
		r * h * d * ((sz - 1) `div` algn + 1) * algn
		where
		sz = fromIntegral $ sizeOf @(IsImagePixel img) undefined
		algn = objectAlignment @((Image algn img nm))

class SizeAlignment obj => StoreObject v (obj :: Object) where
	storeObject :: Ptr (TypeOfObject obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (TypeOfObject obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

instance (Storable t, KnownNat _algn) => StoreObject t ((Atom _algn t _nm)) where
	storeObject p (ObjectLengthAtom) x = poke p x
	loadObject p (ObjectLengthAtom) = peek p
	objectLength _ = ObjectLengthAtom

instance (
	KnownNat _algn,
	MonoFoldable v, Seq.IsSequence v,
	Storable t, Element v ~ t ) =>
	StoreObject v ((List _algn t _nm)) where
	storeObject p ((ObjectLengthList n)) xs =
		pokeArray p . take (fromIntegral n) $ otoList xs
	loadObject p ((ObjectLengthList n)) =
		Seq.fromList <$> peekArray (fromIntegral n) p
	objectLength = ObjectLengthList . fromIntegral . olength

instance (KnownNat algn, IsImage img, Storable (IsImagePixel img)) =>
	StoreObject img ((Image algn img nm)) where
	storeObject p0 (ObjectLengthImage r w _h _d) img =
		for_ (zip (iterate (`plusPtr` fromIntegral s) p0) $ isImageBody img)
			\(p, take (fromIntegral w) -> rw) -> pokeArray (castPtr p) $ take (fromIntegral w) rw
		where s = r * fromIntegral (sizeOf @(IsImagePixel img) undefined)
	loadObject p0 (ObjectLengthImage r w h d) = isImageMake w h d
		<$> for (take (fromIntegral (h * d)) $ iterate (`plusPtr` fromIntegral s) p0) \p -> peekArray (fromIntegral w) (castPtr p)
		where s = r * fromIntegral (sizeOf @(IsImagePixel img) undefined)
	objectLength img = ObjectLengthImage
		(isImageRow img) (isImageWidth img) (isImageHeight img) (isImageDepth img)

class IsImage img where
	type IsImagePixel img
	type ImageFormat img :: T.Format
	isImageRow :: img -> Device.M.Size
	isImageWidth :: img -> Device.M.Size
	isImageHeight :: img -> Device.M.Size
	isImageDepth :: img -> Device.M.Size
	isImageBody :: img -> [[IsImagePixel img]]
	isImageMake :: Device.M.Size -> Device.M.Size -> Device.M.Size -> [[IsImagePixel img]] -> img
