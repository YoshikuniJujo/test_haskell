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

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.Word
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.Type
import Gpu.Vulkan.DescriptorSet.TypeLevel.Copy qualified as Copy

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

data Copy n sdss
	(slbtss :: (Type, [Layout.BindingType])) sdsd
	(slbtsd :: (Type, [Layout.BindingType]))
	(bts :: Layout.BindingType) (is :: Nat) (id :: Nat) = Copy {
	copyNextNew :: TMaybe.M n,
	copySrcSetNew :: D sdss slbtss,
	copyDstSetNew :: D sdsd slbtsd }

class M.CopyListToCore (CopyNextsNew copyArgs) =>
	CopyListToMiddle copyArgs where
	type CopyNextsNew copyArgs :: [Maybe Type]
	copyListToMiddleNew ::
		HeteroParList.PL (U8 Copy) copyArgs ->
		HeteroParList.PL M.Copy (CopyNextsNew copyArgs)

instance CopyListToMiddle '[] where
	type CopyNextsNew '[] = '[]
	copyListToMiddleNew HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked (TMaybe.M n),
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id, Copy.BindingLength bts,
	CopyListToMiddle copyArgs ) =>
	CopyListToMiddle (
		'(n, sdss, '(sls, btss), sdsd, '(sld, btsd), bts, is, id) ':
		copyArgs) where
	type CopyNextsNew (
		'(n, sdss, '(sls, btss), sdsd, '(sld, btsd), bts, is, id) ':
		copyArgs ) = n ': CopyNextsNew copyArgs
	copyListToMiddleNew (U8 c :** cs) = copyToMiddleNew c :** copyListToMiddleNew cs

copyToMiddleNew :: (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id, Copy.BindingLength bts ) =>
	Copy n sdss '(sls, btss) sdsd '(sld, btsd) bts is id -> M.Copy n
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

getCopyArgsNew :: forall n sdss sls btss sdsd sld btsd bts is id . (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id,
	Copy.BindingLength bts ) =>
	Copy n sdss '(sls, btss) sdsd '(sld, btsd) bts is id ->
	(Word32, Word32, Word32, Word32, Word32)
getCopyArgsNew _ = let
	(sb, sae) = Copy.bindingAndArrayElement @btss @bts @is
	(db, dae) = Copy.bindingAndArrayElement @btsd @bts @id in
	(sb, sae, db, dae, Copy.bindingLength @bts)
