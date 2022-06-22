{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout (
	L, create ) where

import Foreign.Pointable
import Control.Exception
import Data.Kind.Object
import Data.HeteroList
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.DescriptorSetLayout.Type
import Gpu.Vulkan.DescriptorSetLayout.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as M
import qualified Gpu.Vulkan.Sampler as Sampler

create :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> M.CreateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . L s -> IO a) -> IO a
create (Device.D dvc) ci macc macd f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . L)

data BindingType = Buffer [Object] | Other

data Binding (bt :: BindingType) where
	BindingBuffer :: {
		bindingBufferDescriptorType :: Descriptor.Type,
		bindingBufferStageFlags :: ShaderStageFlags
		} -> Binding ('Buffer objs)
	BindingOther :: {
		bindingOtherDescriptorType :: Descriptor.Type,
		bindingOtherDescriptorCountOrImmutableSamplers ::
			Either Word32 [Sampler.S],
		bindingOtherStageFlags :: ShaderStageFlags
		} -> Binding 'Other

data CreateInfo n bts = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoBindings :: HeteroVarList Binding bts }

deriving instance (Show n, Show (HeteroVarList Binding bts)) =>
	Show (CreateInfo n bts)

deriving instance Show (Binding bt)
