{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GetSizeAlignment where

import GHC.Generics
import Foreign.Storable

import TryGenerics
import LambdaCube

class GetSizeAlignmentList a where
	getSizeAlignmentList :: [(Int, Int)]

	default getSizeAlignmentList :: (Generic a, MapTypeVal2 Storable (TypeList (Rep a))) => [(Int, Int)]
	getSizeAlignmentList = typeSizeAlignments @(TypeList (Rep a))

instance (Storable a, Storable b, Storable c, Storable d) =>
	GetSizeAlignmentList (a, b, c, d)

class GetSizeAlignmentListUntil t a where
	getSizeAlignmentListUntil :: [(Int, Int)]

	default getSizeAlignmentListUntil :: (
		Generic a,
		MapTypeVal2 Storable (FromJust (TypeUntil t (TypeList (Rep a))))) => [(Int, Int)]
	getSizeAlignmentListUntil = typeSizeAlignments @(FromJust (TypeUntil t (TypeList (Rep a))))

instance (MapTypeVal2 Storable (FromJust (TypeUntil t '[a, b, c, d]))) =>
	GetSizeAlignmentListUntil t (a, b, c, d)
