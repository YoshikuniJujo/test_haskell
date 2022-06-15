{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet where

import Foreign.Pointable
import Data.Word

import qualified Vulkan.Device.Type as Device
import qualified Vulkan.DescriptorPool.Type as Descriptor.Pool
import qualified Vulkan.DescriptorSetLayout.Type as Layout
import qualified Vulkan.DescriptorSet.Middle as M

data AllocateInfo n sp sl = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoDescriptorSetCountOrSetLayouts :: Either Word32 [Layout.L sl] }
	deriving Show

allocateInfoToMiddle :: AllocateInfo n sp sl -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Descriptor.Pool.P dp,
	allocateInfoDescriptorSetCountOrSetLayouts =
		((Layout.unL <$>) <$>) -> dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoDescriptorSetCountOrSetLayouts = dscsls }

newtype S sd sp sl = S M.S deriving Show

allocateSs :: Pointable n =>
	Device.D sd -> AllocateInfo n sp sl -> IO [S sd sp sl]
allocateSs (Device.D dvc) ai = (S <$>) <$> M.allocateSs dvc (allocateInfoToMiddle ai)
