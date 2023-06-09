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

module Data.Kind.ObjectNew where

import GHC.TypeLits
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Kind
import Data.MonoTraversable
import Data.Proxy
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Data.Sequences as Seq

data Object = Atom Alignment Type (Maybe Symbol) | List Alignment Type Symbol

type family ObjectAlignment obj where
	ObjectAlignment (Atom algn t nm) = algn
	ObjectAlignment (List algn t nm) = algn

type Alignment = Nat

type family ObjectType obj where
	ObjectType ('Atom _algn t _nm) = t
	ObjectType ('List _algn t _nm) = t

data ObjectLength (obj :: Object) where
	ObjectLengthAtom :: ObjectLength ('Atom _algn t nm)
	ObjectLengthList :: Int -> ObjectLength ('List _algn t nm)

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

minimumAlignment :: Int
-- minimumAlignment = 256
minimumAlignment = 1

class StoreObject v (obj :: Object) where
	storeObject :: Ptr (ObjectType obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (ObjectType obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

wholeSizeNew :: SizeAlignmentList objs =>
	HeteroParList.PL ObjectLength objs -> Size
wholeSizeNew = wholeSizeFromSizeAlignmentList 0 . sizeAlignmentList

wholeSizeFromSizeAlignmentList :: Size -> HeteroParList.PL SizeAlignmentOfObj objs -> Size
wholeSizeFromSizeAlignmentList sz0 HeteroParList.Nil = sz0
wholeSizeFromSizeAlignmentList sz0 (SizeAlignmentOfObj sz algn :** saoo) =
	wholeSizeFromSizeAlignmentList
		(((sz0 - 1) `div` algn + 1) * algn + sz) saoo

data SizeAlignmentOfObj (obj :: Object) = SizeAlignmentOfObj Size ObjAlignment deriving Show

type Size = Int
type ObjAlignment = Int

class SizeAlignmentList objs where
	sizeAlignmentList :: HeteroParList.PL ObjectLength objs ->
		HeteroParList.PL SizeAlignmentOfObj objs

instance SizeAlignmentList '[] where
	sizeAlignmentList HeteroParList.Nil = HeteroParList.Nil

instance (
	SizeAlignment obj, SizeAlignmentList objs ) =>
	SizeAlignmentList (obj ': objs) where
	sizeAlignmentList (ln :** lns) =
		SizeAlignmentOfObj (objectSize ln) (objectAlignment @obj) :**
		sizeAlignmentList lns

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
