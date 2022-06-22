{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet where

import Foreign.Pointable

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorPool.Type as Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

data AllocateInfo n sp sl = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts :: [Layout.L sl] }
	deriving Show

allocateInfoToMiddle :: AllocateInfo n sp sl -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Descriptor.Pool.P dp,
	allocateInfoSetLayouts = (Layout.unL <$>) -> dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoSetLayouts = dscsls }

newtype S sd sp sl = S M.S deriving Show

allocateSs :: Pointable n =>
	Device.D sd -> AllocateInfo n sp sl -> IO [S sd sp sl]
allocateSs (Device.D dvc) ai = (S <$>) <$> M.allocateSs dvc (allocateInfoToMiddle ai)
