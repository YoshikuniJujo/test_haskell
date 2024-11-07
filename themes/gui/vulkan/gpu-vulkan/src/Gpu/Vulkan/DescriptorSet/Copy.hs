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

	-- * COPY

	Copy(..),

	-- ** CopyListToMiddle

	CopyListToMiddle(..),

	-- ** BindingAndArrayElem

	BindingAndArrayElem,

	-- ** BindingLength

	BindingLength

	) where

import Data.TypeLevel.List qualified as TList
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem.Buffer

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.Type

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Lyt
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

-- * COPY

data Copy mn sdss slbtss (is :: Nat) sdsd slbtsd (id :: Nat)
	(lbts :: Lyt.BindingType) = Copy {
	copyNext :: TMaybe.M mn,
	copySrcSet :: D sdss slbtss, copyDstSet :: D sdsd slbtsd }

-- ** CopyListToMiddle

class M.CopyListToCore (TMapIndex.M0_8 cargs) =>
	CopyListToMiddle cargs where
	copyListToMiddle ::
		HeteroParList.PL (U8 Copy) cargs ->
		HeteroParList.PL M.Copy (TMapIndex.M0_8 cargs)

instance CopyListToMiddle '[] where
	copyListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked (TMaybe.M mn),
	BindingAndArrayElem (TIndex.I1_2 slbtss) lbts is,
	BindingAndArrayElem (TIndex.I1_2 slbtsd) lbts id,
	BindingLength lbts, CopyListToMiddle cargs ) =>
	CopyListToMiddle (
		'(mn, sdss, slbtss, is, sdsd, slbtsd, id, lbts) ': cargs) where
	copyListToMiddle (U8 c :** cs) = copyToMiddle c :** copyListToMiddle cs

copyToMiddle :: forall mn sdss slbtss is sdsd slbtsd id lbts . (
	BindingAndArrayElem (TIndex.I1_2 slbtss) lbts is,
	BindingAndArrayElem (TIndex.I1_2 slbtsd) lbts id, BindingLength lbts ) =>
	Copy mn sdss slbtss is sdsd slbtsd id lbts -> M.Copy mn
copyToMiddle Copy {
	copyNext = mnxt, copySrcSet = D _ ss, copyDstSet = D _ sd } = let
	(bs, aes) = bindingAndArrayElem @(TIndex.I1_2 slbtss) @lbts @is
	(bd, aed) = bindingAndArrayElem @(TIndex.I1_2 slbtsd) @lbts @id
	cnt = bindingLength @lbts in
	M.Copy {
		M.copyNext = mnxt,
		M.copySrcSet = ss,
		M.copySrcBinding = bs, M.copySrcArrayElement = aes,
		M.copyDstSet = sd,
		M.copyDstBinding = bd, M.copyDstArrayElement = aed,
		M.copyDescriptorCount = cnt }

-- ** BindingAndArrayElem

class BindingAndArrayElem
	(lbts :: [Lyt.BindingType])
	(lbt :: Lyt.BindingType) (i :: Nat) where
	bindingAndArrayElem :: Integral n => (n, n)

instance BindingAndArrayElemBuffer lbts objs i =>
	BindingAndArrayElem lbts (Lyt.Buffer objs) i where
	bindingAndArrayElem = bindingAndArrayElemBuffer @lbts @objs @i 0 0

instance BindingAndArrayElemImage lbts iargs i =>
	BindingAndArrayElem lbts (Lyt.Image iargs) i where
	bindingAndArrayElem = bindingAndArrayElemImage @lbts @iargs @i 0 0

instance BindingAndArrayElemImageWithImmutableSampler lbts (TMapIndex.M0'1_3 iargs) i =>
	BindingAndArrayElem lbts (Lyt.ImageSampler iargs) i where
	bindingAndArrayElem =
		bindingAndArrayElemImageWithImmutableSampler @lbts @(TMapIndex.M0'1_3 iargs) @i 0 0

instance BindingAndArrayElemBufferView lbts bvargs i =>
	BindingAndArrayElem lbts (Lyt.BufferView bvargs) i where
	bindingAndArrayElem = bindingAndArrayElemBufferView @lbts @bvargs @i 0 0

-- ** BindingLength

class BindingLength (bt :: Lyt.BindingType) where
	bindingLength :: Integral n => n

instance TList.Length objs => BindingLength (Lyt.Buffer objs) where
	bindingLength = TList.length @_ @objs

instance TList.Length iargs => BindingLength (Lyt.Image iargs) where
	bindingLength = TList.length @_ @iargs

instance TList.Length bvargs => BindingLength (Lyt.BufferView bvargs) where
	bindingLength = TList.length @_ @bvargs

instance TList.Length isargs => BindingLength (Lyt.ImageSampler isargs) where
	bindingLength = TList.length @_ @isargs
