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

	-- * CREATE

	create, D, CreateInfo(..),

	-- ** Binding

	Binding(..), BindingListToMiddle,

	-- ** BindingType

	BindingType(..), BindingTypeListBufferOnlyDynamics,

	-- * ENUM

	module Gpu.Vulkan.DescriptorSetLayout.Enum

	) where

import Prelude hiding (length)

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.List
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.DescriptorSetLayout.Type
import Gpu.Vulkan.DescriptorSetLayout.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as M
import qualified Gpu.Vulkan.Sampler.Type as Sampler

create :: (
	WithPoked (TMaybe.M mn), BindingListToMiddle bts,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> CreateInfo mn bts ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . D s bts -> IO a) -> IO a
create (Device.D dvc) ci (AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create dvc (createInfoToMiddle ci) mac)
		(\l -> M.destroy dvc l mac) (f . D)

data CreateInfo mn bts = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoBindings :: HeteroParList.PL Binding bts }

-- deriving instance (Show (TMaybe.M mn), Show (HeteroParList.PL Binding bts)) =>
--	Show (CreateInfo mn bts)

createInfoToMiddle ::
	BindingListToMiddle bts => CreateInfo mn bts -> M.CreateInfo mn
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoBindings = bds } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoBindings = bindingListToMiddle bds 0 }

data Binding (bt :: BindingType) where
	BindingBuffer :: {
		bindingBufferDescriptorType :: Descriptor.Type,
		bindingBufferStageFlags :: ShaderStageFlags
		} -> Binding ('Buffer objs)
	BindingBufferView :: {
		bindingBufferViewDescriptorType :: Descriptor.Type,
		bindingBufferViewStageFlags :: ShaderStageFlags
		} -> Binding ('BufferView bvargs)
	BindingImage :: {
		bindingImageDescriptorType :: Descriptor.Type,
		bindingImageStageFlags :: ShaderStageFlags
		} -> Binding ('Image iargs)
	BindingImageSampler :: {
		bindingImageSamplerDescriptorType :: Descriptor.Type,
		bindingImageSamplerStageFlags :: ShaderStageFlags,
		bindingImageSamplerImmutableSamplers ::
			HeteroParList.PL Sampler.S (TMapIndex.M2_3 iargs)
		} -> Binding ('ImageSampler iargs)

class BindingListToMiddle bts where
	bindingListToMiddle ::
		HeteroParList.PL Binding bts -> Word32 -> [M.Binding]

instance BindingListToMiddle '[] where
	bindingListToMiddle HeteroParList.Nil _ = []

instance (BindingToMiddle bt, BindingListToMiddle bts) =>
	BindingListToMiddle (bt ': bts) where
	bindingListToMiddle (bd :** bds) bb =
		bindingToMiddle bd bb : bindingListToMiddle bds (bb + 1)

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

instance Length bvargs => BindingToMiddle ('BufferView bvargs) where
	bindingToMiddle BindingBufferView {
		bindingBufferViewDescriptorType = dt,
		bindingBufferViewStageFlags = sfs } bb = M.Binding {
			M.bindingBinding = bb,
			M.bindingDescriptorType = dt,
			M.bindingDescriptorCountOrImmutableSamplers =
				Left (length @_ @bvargs),
			M.bindingStageFlags = sfs }

instance Length iargs => BindingToMiddle ('Image iargs) where
	bindingToMiddle BindingImage {
		bindingImageDescriptorType = dt,
		bindingImageStageFlags = sfs } bb = M.Binding {
			M.bindingBinding = bb,
			M.bindingDescriptorType = dt,
			M.bindingDescriptorCountOrImmutableSamplers =
				Left (length @_ @iargs),
			M.bindingStageFlags = sfs }

instance BindingToMiddle ('ImageSampler iargs) where
	bindingToMiddle BindingImageSampler {
		bindingImageSamplerDescriptorType = dt,
		bindingImageSamplerStageFlags = sfs,
		bindingImageSamplerImmutableSamplers = iss } bb = M.Binding {
			M.bindingBinding = bb,
			M.bindingDescriptorType = dt,
			M.bindingDescriptorCountOrImmutableSamplers = Right
				$ HeteroParList.toList Sampler.sToMiddle iss,
			M.bindingStageFlags = sfs }
