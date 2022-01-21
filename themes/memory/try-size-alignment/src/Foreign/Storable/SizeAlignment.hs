{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE KindSignatures, DataKinds, ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.Storable.SizeAlignment where

import GHC.Generics
import GHC.Generics.TypeFam
import Foreign.Storable
import Data.Kind
import Data.Type.TypeFam
import Data.Type.TypeValMap

type Size = Int
type Alignment = Int
type SizeAlignment = (Size, Alignment)

sizeAlignmentTypeList ::
	forall (as :: [Type]) . MapTypeVal2 Storable as => [SizeAlignment]
sizeAlignmentTypeList = mapTypeVal2 @Storable @as (\x -> (sizeOf x, alignment x))

class SizeAlignmentList a where
	sizeAlignmentList :: [SizeAlignment]

	default sizeAlignmentList :: (
		Generic a,
		MapTypeVal2 Storable (Flatten (Rep a)) ) => [SizeAlignment]
	sizeAlignmentList = sizeAlignmentTypeList @(Flatten (Rep a))

class SizeAlignmentListUntil t a where
	sizeAlignmentListUntil :: [SizeAlignment]

	default sizeAlignmentListUntil :: (
		Generic a,
		MapTypeVal2 Storable (FromJust (Until t (Flatten (Rep a)))) ) =>
		[SizeAlignment]
	sizeAlignmentListUntil =
		sizeAlignmentTypeList @(FromJust (Until t (Flatten (Rep a))))

type MapStorableUntil t ts = MapTypeVal2 Storable (FromJust (Until t ts))

instance (Storable a, Storable b) => SizeAlignmentList (a, b)
instance MapStorableUntil t '[a, b] => SizeAlignmentListUntil t (a, b)
instance (Storable a, Storable b, Storable c) => SizeAlignmentList (a, b, c)
instance MapStorableUntil t '[a, b, c] => SizeAlignmentListUntil t (a, b, c)

data Foo = Foo Bool Int Double deriving (Show, Generic)
instance SizeAlignmentList Foo
instance MapStorableUntil t '[Bool, Int, Double] => SizeAlignmentListUntil t Foo
