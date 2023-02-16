{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
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

import Foreign.Storable
import Control.Exception
import Data.Kind
import Data.TypeLevel
import Data.HeteroParList
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.DescriptorSetLayout.Type
import Gpu.Vulkan.DescriptorSetLayout.Enum

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as M
import qualified Gpu.Vulkan.Sampler as Sampler
import qualified Gpu.Vulkan.Sampler.Middle as Sampler.M

create'' :: (Storable n, Storable c, Storable d) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . L'' s -> IO a) -> IO a
create'' (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . L'')

create :: (Storable n, BindingsToMiddle bts, Storable c, Storable d) =>
	Device.D sd -> CreateInfo n bts ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . L s bts -> IO a) -> IO a
create dvc ci macc macd f =
	create'' dvc (createInfoToMiddle ci) macc macd \(L'' l) -> f $ L l

data Binding (bt :: BindingType) where
	BindingBuffer :: {
		bindingBufferDescriptorType :: Descriptor.Type,
		bindingBufferStageFlags :: ShaderStageFlags
		} -> Binding ('Buffer objs)
	BindingImage :: {
		bindingImageDescriptorType :: Descriptor.Type,
		bindingImageStageFlags :: ShaderStageFlags
		} -> Binding ('Image fmts)
	BindingImageSampler :: {
		bindingImageSamplerDescriptorType :: Descriptor.Type,
		bindingImageSamplerStageFlags :: ShaderStageFlags,
		bindingImageSamplerImmutableSamplers ::
			HeteroParList Sampler.S (MapSnd (fmtss :: [(T.Format, Type)]))
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
				$ heteroParListToList Sampler.sToMiddle iss,
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
	bindingsToMiddle :: HeteroParList Binding bts -> Word32 -> [M.Binding]

instance BindingsToMiddle '[] where bindingsToMiddle HNil _ = []

instance (BindingToMiddle bt, BindingsToMiddle bts) =>
	BindingsToMiddle (bt ': bts) where
	bindingsToMiddle (bd :...: bds) bb =
		bindingToMiddle bd bb : bindingsToMiddle bds (bb + 1)

data CreateInfo n bts = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoBindings :: HeteroParList Binding bts }

createInfoToMiddle :: BindingsToMiddle bts => CreateInfo n bts -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoBindings = bds } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoBindings = bindingsToMiddle bds 0 }

deriving instance (Show n, Show (HeteroParList Binding bts)) =>
	Show (CreateInfo n bts)

-- deriving instance Show (Binding bt)
