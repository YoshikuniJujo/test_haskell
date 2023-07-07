{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Copy (

	-- ** Copy

	Copy(..), CopyListToMiddle(..)

	) where

import Data.TypeLevel.List qualified as TypeLevel.Length
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem qualified as Common
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem.Buffer qualified as Common

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.Word
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.Type

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

data Copy n sdss
	(slbtss :: (Type, [Layout.BindingType])) sdsd
	(slbtsd :: (Type, [Layout.BindingType]))
	(bts :: Layout.BindingType) (is :: Nat) (id :: Nat) = Copy {
	copyNextNew :: TMaybe.M n,
	copySrcSetNew :: D sdss slbtss,
	copyDstSetNew :: D sdsd slbtsd }

class M.CopyListToCore (TMapIndex.M0_8 copyArgs) =>
	CopyListToMiddle copyArgs where
	copyListToMiddleNew ::
		HeteroParList.PL (U8 Copy) copyArgs ->
		HeteroParList.PL M.Copy (TMapIndex.M0_8 copyArgs)

instance CopyListToMiddle '[] where
	copyListToMiddleNew HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked (TMaybe.M n),
	BindingAndArrayElement (TIndex.I1_2 slbtss) bts is,
	BindingAndArrayElement (TIndex.I1_2 slbtsd) bts id,
	BindingLength bts, CopyListToMiddle copyArgs ) =>
	CopyListToMiddle (
		'(n, sdss, slbtss, sdsd, slbtsd, bts, is, id) ':
		copyArgs) where
	copyListToMiddleNew (U8 c :** cs) = copyToMiddleNew c :** copyListToMiddleNew cs

copyToMiddleNew :: (
	BindingAndArrayElement (TIndex.I1_2 slbtss) bts is,
	BindingAndArrayElement (TIndex.I1_2 slbtsd) bts id, BindingLength bts ) =>
	Copy n sdss slbtss sdsd slbtsd bts is id -> M.Copy n
copyToMiddleNew c@Copy {
	copyNextNew = mnxt, copySrcSetNew = D _ ss, copyDstSetNew = D _ ds } = let
	(sb, sae, db, dae, cnt) = getCopyArgsNew c in
	M.Copy {
		M.copyNext = mnxt,
		M.copySrcSet = ss,
		M.copySrcBinding = sb,
		M.copySrcArrayElement = sae, M.copyDstSet = ds,
		M.copyDstBinding = db,
		M.copyDstArrayElement = dae, M.copyDescriptorCount = cnt }

getCopyArgsNew :: forall n sdss slbtss sdsd slbtsd bts is id . (
	BindingAndArrayElement (TIndex.I1_2 slbtss) bts is,
	BindingAndArrayElement (TIndex.I1_2 slbtsd) bts id,
	BindingLength bts ) =>
	Copy n sdss slbtss sdsd slbtsd bts is id ->
	(Word32, Word32, Word32, Word32, Word32)
getCopyArgsNew _ = let
	(sb, sae) = bindingAndArrayElement @(TIndex.I1_2 slbtss) @bts @is
	(db, dae) = bindingAndArrayElement @(TIndex.I1_2 slbtsd) @bts @id in
	(sb, sae, db, dae, bindingLength @bts)

class BindingAndArrayElement
	(bts :: [Layout.BindingType])
	(bt :: Layout.BindingType) (i :: Nat) where
	bindingAndArrayElement :: Integral n => (n, n)

instance Common.BindingAndArrayElem bts vobjs i =>
	BindingAndArrayElement bts (Layout.Buffer vobjs) i where
	bindingAndArrayElement = Common.bindingAndArrayElem @bts @vobjs @i 0 0

instance Common.BindingAndArrayElemBufferView bts nmts i =>
	BindingAndArrayElement bts (Layout.BufferView nmts) i where
	bindingAndArrayElement =
		Common.bindingAndArrayElemBufferView @bts @nmts @i 0 0

instance
	Common.BindingAndArrayElemImage bts imgs i =>
	BindingAndArrayElement bts (Layout.Image imgs) i where
	bindingAndArrayElement = Common.bindingAndArrayElemImage @bts @imgs @i 0 0

class BindingLength (bt :: Layout.BindingType) where
	bindingLength :: Integral n => n

instance TypeLevel.Length.Length vobjs =>
	BindingLength (Layout.Buffer vobjs) where
	bindingLength = TypeLevel.Length.length @_ @vobjs
