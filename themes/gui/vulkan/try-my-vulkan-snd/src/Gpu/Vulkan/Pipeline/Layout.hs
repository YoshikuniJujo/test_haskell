{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout (
	L, create, CreateInfo(..),
	M.CreateFlags, pattern M.CreateFlagsZero ) where

import Foreign.Pointable
import Control.Exception
import Data.HeteroList

import Gpu.Vulkan.Pipeline.Layout.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Descriptor.Set.Layout
import qualified Gpu.Vulkan.PushConstant as PushConstant
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as M

data CreateInfo n ss = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlags,
	createInfoSetLayouts :: HeteroVarList Descriptor.Set.Layout.L ss,
	createInfoPushConstantRanges :: [PushConstant.Range] }

deriving instance (Show n, Show (HeteroVarList Descriptor.Set.Layout.L ss)) =>
	Show (CreateInfo n ss)

createInfoToMiddle :: CreateInfo n ss -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts =
		heteroVarListToList Descriptor.Set.Layout.unL -> sls,
	createInfoPushConstantRanges = pcrs
	} = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoSetLayouts = sls,
		M.createInfoPushConstantRanges = pcrs }

create :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> CreateInfo n ss ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . L s -> IO a) -> IO a
create (Device.D dvc) (createInfoToMiddle -> ci) macc macd f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . L)
