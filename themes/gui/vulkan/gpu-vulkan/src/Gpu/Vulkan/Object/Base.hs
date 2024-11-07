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

	Atom, AtomNew, List, Image,
	AtomNoName, ListNoName, ImageNoName,
	AtomMaybeName, ListMaybeName, ImageMaybeName,

	-- ** Type Of Object

	TypeOf,

	-- * OBJECT LENGTH

	Length(..), renameLength,

	-- * STORE OBJECT

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

-- OBJECT

data O = O Alignment (Maybe Symbol) ObjectType Type

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

type Atom algn t mnm = AtomMaybeName algn t mnm
type AtomNew algn t nm = AtomMaybeName algn t ('Just nm)
type List algn t nm = ListMaybeName algn t ('Just nm)
type Image algn t nm = ImageMaybeName algn t ('Just nm)

type AtomNoName al t = AtomMaybeName al t 'Nothing
type ListNoName al t = ListMaybeName al t 'Nothing
type ImageNoName al t = ImageMaybeName al t 'Nothing

type AtomMaybeName al t mnm = 'O al mnm AtomT t
type ListMaybeName al t mnm = 'O al mnm ListT t
type ImageMaybeName al t mnm = 'O al mnm ImageT t

-- Type of Object

type family TypeOf obj where
	TypeOf ((Atom _algn t _nm)) = t
	TypeOf ((List _algn t _nm)) = t

-- OBJECT LENGTH

data Length (obj :: O) where
	LengthAtom :: Length (Atom algn t nm)
	LengthList :: Device.M.Size -> Length ('O algn mnm ListT t)
	LengthImage :: {
		lengthImageRow :: Device.M.Size,
		lengthImageWidth :: Device.M.Size,
		lengthImageHeight :: Device.M.Size,
		lengthImageDepth :: Device.M.Size } ->
		Length ('O algn mnm ImageT t)

deriving instance Eq (Length obj)
deriving instance Show (Length obj)

instance Default (Length (Atom algn t mnm)) where def = LengthAtom
instance Default (Length (List algn t nm)) where def = LengthList 0
instance Default (Length (Image algn t nm)) where def = LengthImage 0 0 0 0

renameLength :: Length ('O algn mnm ot t) -> Length ('O algn mnm' ot t)
renameLength LengthAtom = LengthAtom
renameLength (LengthList n) = LengthList n
renameLength (LengthImage r w h d) = LengthImage r w h d

-- STORE OBJECT

class SizeAlignment obj => Store v (obj :: O) where
	store :: Ptr (TypeOf obj) -> Length obj -> v -> IO ()
	load :: Ptr (TypeOf obj) -> Length obj -> IO v
	length :: v -> Length obj

instance (S.Storable t, KnownNat algn) => Store t ((Atom algn t _nm)) where
	store p (LengthAtom) x = S.poke p x
	load p (LengthAtom) = S.peek p
	length _ = LengthAtom

instance (KnownNat algn, Seq.IsSequence v, S.Storable t, Element v ~ t) =>
	Store v ((List algn t _nm)) where
	store p ((LengthList (fromIntegral -> n))) xs =
		pokeArray p . take n $ otoList xs
	load p (LengthList (fromIntegral -> n)) = Seq.fromList <$> peekArray n p
	length = LengthList . fromIntegral . olength

instance (KnownNat algn, IsImage img) =>
	Store img ((Image algn img nm)) where
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

-- SIZE AND ALIGNMENT

class SizeAlignment obj where
	size :: Length obj -> Device.M.Size
	alignment :: Device.M.Size

instance (KnownNat algn, S.Storable t) =>
	SizeAlignment ((AtomMaybeName algn t _nm)) where
	size (LengthAtom) =
		applyAlign algn . fromIntegral $ S.sizeOf @t undefined
		where algn = alignment @((AtomMaybeName algn t _nm))
	alignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (S.alignment @t undefined)

instance (KnownNat algn, S.Storable t) => SizeAlignment (ListMaybeName algn t _nm) where
	size (LengthList n) = applyAlign algn' $ n * applyAlign algn sz
		where
		sz = fromIntegral $ S.sizeOf @t undefined
		algn = fromIntegral $ S.alignment @t undefined
		algn' = alignment @((ListMaybeName algn t _nm))
	alignment = fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (S.alignment @t undefined)

instance (KnownNat algn, S.Storable (ImagePixel img)) =>
	SizeAlignment ((ImageMaybeName algn img nm)) where
	size (LengthImage r _w h d) = r * h * d * applyAlign algn sz
		where
		sz = fromIntegral $ S.sizeOf @(ImagePixel img) undefined
		algn = alignment @((ImageMaybeName algn img nm))
	alignment =fromIntegral (natVal (Proxy :: Proxy algn)) `lcm`
		fromIntegral (S.alignment @(ImagePixel img) undefined)

applyAlign :: Integral n => n -> n -> n
applyAlign algn ofst = ((ofst - 1) `div` algn + 1) * algn
