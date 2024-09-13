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

	-- * STORE OBJECT

	Store(..), StoreDyn(..),

	-- * SIZE AND ALIGNMENT

	SizeAlignment(..), SizeAlignmentDyn(..)

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

-- OBJECT

data O = O (Maybe Symbol) ObjectType Type

type Alignment = Nat

data ObjectType = AtomT | ListT | ImageT deriving Show

class (S.Storable (ImagePixel img), T.FormatToValue (ImageFormat img)) =>
	IsImage img where
	type ImagePixel img
	type ImageFormat img :: T.Format
	imageRow :: img -> Device.M.Size
	imageWidth :: img -> Device.M.Size
	imageHeight :: img -> Device.M.Size
	imageDepth :: img -> Device.M.Size
	imageBody :: img -> [[ImagePixel img]]
	imageMake :: Device.M.Size -> Device.M.Size -> Device.M.Size ->
		[[ImagePixel img]] -> img

-- Synonyms

type Atom t mnm = 'O mnm AtomT t
type List t nm = 'O ('Just nm) ListT t
type Image t nm = 'O ('Just nm) ImageT t

-- Type of Object

type family TypeOf obj where
	TypeOf ((Atom t _nm)) = t
	TypeOf ((List t _nm)) = t

-- OBJECT LENGTH

data Length (obj :: O) where
	LengthAtom :: Length (Atom t nm)
	LengthList :: Device.M.Size -> Length ('O mnm ListT t)
	LengthImage :: {
		lengthImageRow :: Device.M.Size,
		lengthImageWidth :: Device.M.Size,
		lengthImageHeight :: Device.M.Size,
		lengthImageDepth :: Device.M.Size } ->
		Length ('O mnm ImageT t)

deriving instance Eq (Length obj)
deriving instance Show (Length obj)

instance Default (Length (Atom t mnm)) where def = LengthAtom
instance Default (Length (List t nm)) where def = LengthList 0
instance Default (Length (Image t nm)) where def = LengthImage 0 0 0 0

renameLength :: Length ('O mnm ot t) -> Length ('O mnm' ot t)
renameLength LengthAtom = LengthAtom
renameLength (LengthList n) = LengthList n
renameLength (LengthImage r w h d) = LengthImage r w h d

-- STORE OBJECT

class SizeAlignment obj => Store v (obj :: O) where
	store :: Ptr (TypeOf obj) -> Length obj -> v -> IO ()
	load :: Ptr (TypeOf obj) -> Length obj -> IO v
	length :: v -> Length obj

instance S.Storable t => Store t ((Atom t _nm)) where
	store p (LengthAtom) x = S.poke p x
	load p (LengthAtom) = S.peek p
	length _ = LengthAtom

instance (Seq.IsSequence v, S.Storable t, Element v ~ t) =>
	Store v ((List t _nm)) where
	store p ((LengthList (fromIntegral -> n))) xs =
		pokeArray p . take n $ otoList xs
	load p (LengthList (fromIntegral -> n)) = Seq.fromList <$> peekArray n p
	length = LengthList . fromIntegral . olength

instance IsImage img =>
	Store img ((Image img nm)) where
	store p0 (LengthImage (fromIntegral -> r) (fromIntegral -> w) _ _) img =
		for_ (zip (iterate (`plusPtr` s) p0) $ imageBody img)
			\(p, take w -> rw) -> pokeArray (castPtr p) rw
		where s = r * S.sizeOf @(ImagePixel img) undefined
	load p0 (LengthImage (fromIntegral -> r)
		w_@(fromIntegral -> w) h_@(fromIntegral -> h)
		d_@(fromIntegral -> d)) =
		imageMake w_ h_ d_
			<$> for (take (h * d) $ iterate (`plusPtr` s) p0) \p ->
				peekArray w (castPtr p)
		where s = r * (S.sizeOf @(ImagePixel img) undefined)
	length img = LengthImage
		(imageRow img) (imageWidth img) (imageHeight img)
		(imageDepth img)

class SizeAlignmentDyn algn obj => StoreDyn v algn (obj :: O) where
	storeDyn :: Ptr (TypeOf obj) -> Length obj -> v -> IO ()
	loadDyn :: Ptr (TypeOf obj) -> Length obj -> IO v
	lengthDyn :: v -> Length obj

instance (KnownNat algn, S.Storable t) => StoreDyn t algn ((Atom t _nm)) where
	storeDyn p (LengthAtom) x = S.poke p x
	loadDyn p (LengthAtom) = S.peek p
	lengthDyn _ = LengthAtom

instance (KnownNat algn, Seq.IsSequence v, S.Storable t, Element v ~ t) =>
	StoreDyn v algn ((List t _nm)) where
	storeDyn p ((LengthList (fromIntegral -> n))) xs =
		pokeArray p . take n $ otoList xs
	loadDyn p (LengthList (fromIntegral -> n)) = Seq.fromList <$> peekArray n p
	lengthDyn = LengthList . fromIntegral . olength

instance (KnownNat algn, IsImage img) =>
	StoreDyn img algn ((Image img nm)) where
	storeDyn p0 (LengthImage (fromIntegral -> r) (fromIntegral -> w) _ _) img =
		for_ (zip (iterate (`plusPtr` s) p0) $ imageBody img)
			\(p, take w -> rw) -> pokeArray (castPtr p) rw
		where s = r * S.sizeOf @(ImagePixel img) undefined
	loadDyn p0 (LengthImage (fromIntegral -> r)
		w_@(fromIntegral -> w) h_@(fromIntegral -> h)
		d_@(fromIntegral -> d)) =
		imageMake w_ h_ d_
			<$> for (take (h * d) $ iterate (`plusPtr` s) p0) \p ->
				peekArray w (castPtr p)
		where s = r * (S.sizeOf @(ImagePixel img) undefined)
	lengthDyn img = LengthImage
		(imageRow img) (imageWidth img) (imageHeight img)
		(imageDepth img)

-- SIZE AND ALIGNMENT

class SizeAlignment obj where
	size :: Length obj -> Device.M.Size
	alignment :: Device.M.Size

instance S.Storable t =>
	SizeAlignment ((Atom t _nm)) where
	size (LengthAtom) =
		applyAlign algn . fromIntegral $ S.sizeOf @t undefined
		where algn = alignment @((Atom t _nm))
	alignment = fromIntegral (S.alignment @t undefined)

instance S.Storable t => SizeAlignment (List t _nm) where
	size (LengthList n) = applyAlign algn' $ n * applyAlign algn sz
		where
		sz = fromIntegral $ S.sizeOf @t undefined
		algn = fromIntegral $ S.alignment @t undefined
		algn' = alignment @((List t _nm))
	alignment = fromIntegral (S.alignment @t undefined)

instance S.Storable (ImagePixel img) =>
	SizeAlignment ((Image img nm)) where
	size (LengthImage r _w h d) = r * h * d * applyAlign algn sz
		where
		sz = fromIntegral $ S.sizeOf @(ImagePixel img) undefined
		algn = alignment @((Image img nm))
	alignment = fromIntegral (S.alignment @(ImagePixel img) undefined)

class SizeAlignmentDyn (algn :: Nat) obj where
	sizeDyn :: Length obj -> Device.M.Size
	alignmentDyn :: Device.M.Size

instance (KnownNat algn, S.Storable t) =>
	SizeAlignmentDyn algn ((Atom t _nm)) where
	sizeDyn (LengthAtom) =
		applyAlign algn . fromIntegral $ S.sizeOf @t undefined
		where algn = alignmentDyn @algn @((Atom t _nm))
	alignmentDyn = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (S.alignment @t undefined)

instance (KnownNat algn, S.Storable t) => SizeAlignmentDyn algn (List t _nm) where
	sizeDyn (LengthList n) = applyAlign algn' $ n * applyAlign algn sz
		where
		sz = fromIntegral $ S.sizeOf @t undefined
		algn = fromIntegral $ S.alignment @t undefined
		algn' = alignmentDyn @algn @((List t _nm))
	alignmentDyn = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (S.alignment @t undefined)

instance (KnownNat algn, S.Storable (ImagePixel img)) =>
	SizeAlignmentDyn algn ((Image img nm)) where
	sizeDyn (LengthImage r _w h d) = r * h * d * applyAlign algn sz
		where
		sz = fromIntegral $ S.sizeOf @(ImagePixel img) undefined
		algn = alignmentDyn @algn @((Image img nm))
	alignmentDyn =fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (S.alignment @(ImagePixel img) undefined)

applyAlign :: Integral n => n -> n -> n
applyAlign algn ofst = ((ofst - 1) `div` algn + 1) * algn
