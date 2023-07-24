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

	-- * OBJECT

	O(..), IsImage(..),

	-- ** Synonyms

	Atom, List, Image,

	-- ** Type Of Object

	TypeOf,

	-- * OBJECT LENGTH

	Length(..), renameLength,

	-- * STORE

	Store(..),

	-- * SIZE AND ALIGNMENT

	SizeAlignment(..)

	) where

import GHC.TypeLits
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable qualified as S
import Data.Kind
import Data.Foldable
import Data.Traversable
import Data.MonoTraversable
import Data.Proxy
import Data.Default

import qualified Data.Sequences as Seq

import Gpu.Vulkan.TypeEnum qualified as T

import Gpu.Vulkan.Device.Middle qualified as Device.M

data O = O Alignment (Maybe Symbol) ObjectType Type

data ObjectType = AtomT | ListT | ImageT deriving Show

type Atom algn t mnm = 'O algn mnm AtomT t
type List algn t nm = 'O algn ('Just nm) ListT t
type Image algn t nm = 'O algn ('Just nm) ImageT t

type Alignment = Nat

data Length (obj :: O) where
	LengthAtom :: Length (Atom algn t nm)
	LengthList :: Device.M.Size -> Length ('O algn mnm ListT t)
	LengthImage :: {
		lengthImageRow :: Device.M.Size,
		lengthImageWidth :: Device.M.Size,
		lengthImageHeight :: Device.M.Size,
		lengthImageDepth :: Device.M.Size } -> Length ('O algn mnm ImageT t)

deriving instance Eq (Length obj)
deriving instance Show (Length obj)

renameLength :: Length ('O algn mnm ot t) ->
	Length ('O algn mnm' ot t)
renameLength LengthAtom = LengthAtom
renameLength (LengthList n) = LengthList n
renameLength (LengthImage r w h d) = LengthImage r w h d

type family TypeOf obj where
	TypeOf ((Atom _algn t _nm)) = t
	TypeOf ((List _algn t _nm)) = t

instance Default (Length (Atom algn t mnm)) where def = LengthAtom
instance Default (Length (List algn t nm)) where def = LengthList 0
instance Default (Length (Image algn t nm)) where def = LengthImage 0 0 0 0

data SizeAlignmentOfObj (obj :: O) =
	SizeAlignmentOfObj Size ObjAlignment deriving Show

type Size = Int
type ObjAlignment = Int

class SizeAlignment obj where
	size :: Length obj -> Device.M.Size
	alignment :: Device.M.Size

applyAlign :: Integral n => n -> n -> n
applyAlign algn ofst = ((ofst - 1) `div` algn + 1) * algn

instance (KnownNat algn, S.Storable t) =>
	SizeAlignment ((Atom algn t _nm)) where
	size (LengthAtom) = applyAlign algn . fromIntegral $ S.sizeOf @t undefined
		where algn = alignment @((Atom algn t _nm))
	alignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (S.alignment @t undefined)

instance (KnownNat algn, S.Storable t) =>
	SizeAlignment ((List algn t _nm)) where
	size (LengthList n) = applyAlign algn' $ n * ((sz - 1) `div` algn + 1) * algn
		where
		sz = fromIntegral $ S.sizeOf @t undefined
		algn = fromIntegral $ S.alignment @t undefined
		algn' = alignment @((List algn t _nm))
	alignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (S.alignment @t undefined)

instance (KnownNat algn, S.Storable (ImagePixel img)) => SizeAlignment ((Image algn img nm)) where
	size (LengthImage r _w h d) =
		r * h * d * ((sz - 1) `div` algn + 1) * algn
		where
		sz = fromIntegral $ S.sizeOf @(ImagePixel img) undefined
		algn = alignment @((Image algn img nm))
	alignment =fromIntegral (natVal (Proxy :: Proxy algn))
		`lcm`
		fromIntegral (S.alignment @(ImagePixel img) undefined)

class SizeAlignment obj => Store v (obj :: O) where
	store :: Ptr (TypeOf obj) -> Length obj -> v -> IO ()
	load :: Ptr (TypeOf obj) -> Length obj -> IO v
	length :: v -> Length obj

instance (S.Storable t, KnownNat _algn) => Store t ((Atom _algn t _nm)) where
	store p (LengthAtom) x = S.poke p x
	load p (LengthAtom) = S.peek p
	length _ = LengthAtom

instance (
	KnownNat _algn,
	MonoFoldable v, Seq.IsSequence v,
	S.Storable t, Element v ~ t ) =>
	Store v ((List _algn t _nm)) where
	store p ((LengthList n)) xs =
		pokeArray p . take (fromIntegral n) $ otoList xs
	load p ((LengthList n)) =
		Seq.fromList <$> peekArray (fromIntegral n) p
	length = LengthList . fromIntegral . olength

instance (KnownNat algn, IsImage img, S.Storable (ImagePixel img)) =>
	Store img ((Image algn img nm)) where
	store p0 (LengthImage r w _h _d) img =
		for_ (zip (iterate (`plusPtr` fromIntegral s) p0) $ imageBody img)
			\(p, take (fromIntegral w) -> rw) -> pokeArray (castPtr p) $ take (fromIntegral w) rw
		where s = r * fromIntegral (S.sizeOf @(ImagePixel img) undefined)
	load p0 (LengthImage r w h d) = imageMake w h d
		<$> for (take (fromIntegral (h * d)) $ iterate (`plusPtr` fromIntegral s) p0) \p -> peekArray (fromIntegral w) (castPtr p)
		where s = r * fromIntegral (S.sizeOf @(ImagePixel img) undefined)
	length img = LengthImage
		(imageRow img) (imageWidth img) (imageHeight img) (imageDepth img)

class IsImage img where
	type ImagePixel img
	type ImageFormat img :: T.Format
	imageRow :: img -> Device.M.Size
	imageWidth :: img -> Device.M.Size
	imageHeight :: img -> Device.M.Size
	imageDepth :: img -> Device.M.Size
	imageBody :: img -> [[ImagePixel img]]
	imageMake :: Device.M.Size -> Device.M.Size -> Device.M.Size -> [[ImagePixel img]] -> img
