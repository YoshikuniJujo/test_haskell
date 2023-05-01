{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout (
	L'', create'', L, create, CreateInfo(..), Binding(..),

	CreateFlags,
	pattern CreateUpdateAfterBindPoolBit,
	pattern CreatePushDescriptorBitKhr, pattern CreateHostOnlyPoolBitValve,
	pattern CreateUpdateAfterBindPoolBitExt, pattern CreateFlagBitsMaxEnum

	) where

import Prelude hiding (length)

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Length
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.DescriptorSetLayout.Type
import Gpu.Vulkan.DescriptorSetLayout.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as M
import qualified Gpu.Vulkan.Sampler as Sampler
import qualified Gpu.Vulkan.Sampler.Middle as Sampler.M

create'' :: (WithPoked (TMaybe.M mn), WithPoked c, WithPoked d) =>
	Device.D sd -> M.CreateInfo mn ->
	Maybe (AllocationCallbacks.A sc c) -> Maybe (AllocationCallbacks.A sd' d) ->
	(forall s . L'' s -> IO a) -> IO a
create'' (Device.D dvc) ci
	((AllocationCallbacks.toMiddle <$>) -> macc)
	((AllocationCallbacks.toMiddle <$>) -> macd) f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . L'')

create :: (WithPoked (TMaybe.M mn), BindingsToMiddle bts, WithPoked c, WithPoked d) =>
	Device.D sd -> CreateInfo mn bts ->
	Maybe (AllocationCallbacks.A sc c) -> Maybe (AllocationCallbacks.A sd d) ->
	(forall s . L s bts -> IO a) -> IO a
create dvc ci macc macd f =
	create'' dvc (createInfoToMiddle ci) macc macd \(L'' l) -> f $ L l

data Binding (bt :: BindingType) where
	BindingBuffer :: {
		bindingBufferDescriptorType :: Descriptor.Type,
		bindingBufferStageFlags :: ShaderStageFlags
		} -> Binding ('Buffer objs)
	BindingBufferView :: {
		bindingBufferViewDescriptorType :: Descriptor.Type,
		bindingBufferViewStageFlags :: ShaderStageFlags
		} -> Binding ('BufferView bvs)
	BindingImage :: {
		bindingImageDescriptorType :: Descriptor.Type,
		bindingImageStageFlags :: ShaderStageFlags
		} -> Binding ('Image fmts)
	BindingImageSampler :: {
		bindingImageSamplerDescriptorType :: Descriptor.Type,
		bindingImageSamplerStageFlags :: ShaderStageFlags,
		bindingImageSamplerImmutableSamplers ::
			HeteroParList.PL Sampler.S (MapSnd (fmtss :: [(T.Format, Type)]))
		} -> Binding ('ImageSampler fmtss)
	BindingOther :: {
		bindingOtherDescriptorType :: Descriptor.Type,
		bindingOtherDescriptorCountOrImmutableSamplers ::
			Either Word32 [Sampler.M.S],
		bindingOtherStageFlags :: ShaderStageFlags
		} -> Binding 'Other

class BindingToMiddle bt where
	bindingToMiddle :: Binding bt -> Word32 -> M.Binding

instance Length objs => BindingToMiddle ('Buffer objs) where
	bindingToMiddle BindingBuffer {
		bindingBufferDescriptorType = dt,
		bindingBufferStageFlags = sfs } bb = M.Binding {
			M.bindingBinding = bb,
			M.bindingDescriptorType = dt,
			M.bindingDescriptorCountOrImmutableSamplers =
				Left (length @_ @objs),
			M.bindingStageFlags = sfs }

instance Length bvs => BindingToMiddle ('BufferView bvs) where
	bindingToMiddle BindingBufferView {
		bindingBufferViewDescriptorType = dt,
		bindingBufferViewStageFlags = sfs } bb = M.Binding {
			M.bindingBinding = bb,
			M.bindingDescriptorType = dt,
			M.bindingDescriptorCountOrImmutableSamplers =
				Left (length @_ @bvs),
			M.bindingStageFlags = sfs }

instance Length fmts => BindingToMiddle ('Image fmts) where
	bindingToMiddle BindingImage {
		bindingImageDescriptorType = dt,
		bindingImageStageFlags = sfs } bb = M.Binding {
			M.bindingBinding = bb,
			M.bindingDescriptorType = dt,
			M.bindingDescriptorCountOrImmutableSamplers =
				Left (length @_ @fmts),
			M.bindingStageFlags = sfs }

instance BindingToMiddle ('ImageSampler fmtss) where
	bindingToMiddle BindingImageSampler {
		bindingImageSamplerDescriptorType = dt,
		bindingImageSamplerStageFlags = sfs,
		bindingImageSamplerImmutableSamplers = iss } bb = M.Binding {
			M.bindingBinding = bb,
			M.bindingDescriptorType = dt,
			M.bindingDescriptorCountOrImmutableSamplers = Right
				$ HeteroParList.toList Sampler.sToMiddle iss,
			M.bindingStageFlags = sfs
			}

instance BindingToMiddle 'Other where
	bindingToMiddle BindingOther {
		bindingOtherDescriptorType = dt,
		bindingOtherDescriptorCountOrImmutableSamplers = cois,
		bindingOtherStageFlags = sfs } bb = M.Binding {
			M.bindingBinding = bb,
			M.bindingDescriptorType = dt,
			M.bindingDescriptorCountOrImmutableSamplers = cois,
			M.bindingStageFlags = sfs }

class BindingsToMiddle bts where
	bindingsToMiddle :: HeteroParList.PL Binding bts -> Word32 -> [M.Binding]

instance BindingsToMiddle '[] where bindingsToMiddle HeteroParList.Nil _ = []

instance (BindingToMiddle bt, BindingsToMiddle bts) =>
	BindingsToMiddle (bt ': bts) where
	bindingsToMiddle (bd :** bds) bb =
		bindingToMiddle bd bb : bindingsToMiddle bds (bb + 1)

data CreateInfo mn bts = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoBindings :: HeteroParList.PL Binding bts }

createInfoToMiddle :: BindingsToMiddle bts => CreateInfo mn bts -> M.CreateInfo mn
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoBindings = bds } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoBindings = bindingsToMiddle bds 0 }

deriving instance (Show (TMaybe.M mn), Show (HeteroParList.PL Binding bts)) =>
	Show (CreateInfo mn bts)

-- deriving instance Show (Binding bt)
