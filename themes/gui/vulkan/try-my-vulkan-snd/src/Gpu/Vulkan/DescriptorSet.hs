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

module Gpu.Vulkan.DescriptorSet (

	-- * ALLOCATE

	allocateDs, D, AllocateInfo(..), DListFromMiddle, DefaultDynamicLengths,

	-- * UPDATE

	updateDs,

	-- ** Write

	W.Write(..), W.WriteListToMiddle,
	W.WriteSources(..), W.WriteSourcesArg(..), W.WriteSourcesToMiddle,

	-- ** Copy

	Copy(..), CopyListToMiddle

	) where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.Default
import Data.Word
import Data.Kind.Object qualified as KObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.Type
import Gpu.Vulkan.DescriptorSet.TypeLevel.Common
import Gpu.Vulkan.DescriptorSet.TypeLevel.Copy qualified as Copy

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorPool.Type as Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as Layout.M
import qualified Gpu.Vulkan.DescriptorSet.Write as W
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import Gpu.Vulkan.Misc

layoutToMiddle :: U2 Layout.L slbts -> Layout.M.L
layoutToMiddle (U2 (Layout.L l)) = l

data AllocateInfo mn sp slbtss = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts :: HeteroParList.PL (U2 Layout.L) slbtss }

deriving instance (Show (TMaybe.M n), Show (HeteroParList.PL (U2 Layout.L) slbtss)) =>
	Show (AllocateInfo n sp slbtss)

allocateInfoToMiddle :: AllocateInfo n sp slbtss -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Descriptor.Pool.P dp,
	allocateInfoSetLayouts = dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoSetLayouts =
			HeteroParList.toList layoutToMiddle dscsls }

class DListFromMiddle slbtss where
	dListFromMiddle :: [M.D] -> IO (HeteroParList.PL (D s) slbtss)

instance DListFromMiddle '[] where
	dListFromMiddle = \case [] -> pure HeteroParList.Nil; _ -> error "bad"

instance (
	DefaultDynamicLengths slbts,
	DListFromMiddle slbtss ) =>
	DListFromMiddle (slbts ': slbtss) where
	dListFromMiddle = \case
		(d : ds) -> (:**)
			<$> ((`D` d) <$> newDefaultIORef)
			<*> dListFromMiddle @slbtss ds
		_ -> error "bad"

type DefaultDynamicLengths slbts = Default
	(HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(LayoutArgOnlyDynamics slbts))

allocateDs :: (WithPoked (TMaybe.M mn), DListFromMiddle slbtss) =>
	Device.D sd -> AllocateInfo mn sp slbtss ->
	(forall s . HeteroParList.PL (D s) slbtss -> IO a) -> IO a
allocateDs (Device.D dvc) ai f = do
	dsm <- M.allocateDs dvc (allocateInfoToMiddle ai)
	ds <- dListFromMiddle dsm
	f ds <* M.freeDs dvc
		((\(Descriptor.Pool.P p) -> p) $ allocateInfoDescriptorPool ai)
		dsm

data Copy n sdss (slbtss :: LayoutArg) sdsd (slbtsd :: LayoutArg)
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

updateDs :: (W.WriteListToMiddle writeArgs, CopyListToMiddle copyArgs) =>
	Device.D sd ->
	HeteroParList.PL (U4 W.Write) writeArgs ->
	HeteroParList.PL (U8 Copy) copyArgs  -> IO ()
updateDs (Device.D dvc) ws cs =
	W.writeListUpdateDynamicLength ws >> M.updateDs dvc ws' cs'
	where ws' = W.writeListToMiddle ws; cs' = copyListToMiddleNew cs
