{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGe FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Buffer where

import Foreign.Pointable
import Control.Exception
import Data.Kind.Object
import Data.HeteroList

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Buffer.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Buffer.Middle as M
import qualified Gpu.Vulkan.Buffer.Core as C
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily

data B s (objs :: [Object]) = B (HeteroVarList ObjectLength objs) C.B

deriving instance Show (HeteroVarList ObjectLength objs) => Show (B s objs)

data CreateInfo n objs = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoLengths :: HeteroVarList ObjectLength objs,
	createInfoUsage :: UsageFlags,
	createInfoSharingMode :: SharingMode,
	createInfoQueueFamilyIndices :: [QueueFamily.Index] }

deriving instance (Show n, Show (HeteroVarList ObjectLength objs)) =>
	Show (CreateInfo n objs)

createInfoToMiddle :: WholeSize objs =>
	CreateInfo n objs -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoLengths = lns,
	createInfoUsage = usg,
	createInfoSharingMode = smd,
	createInfoQueueFamilyIndices = qfis } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoSize = fromIntegral $ wholeSize 0 lns,
	M.createInfoUsage = usg,
	M.createInfoSharingMode = smd,
	M.createInfoQueueFamilyIndices = qfis }

create :: (WholeSize objs, Pointable n, Pointable c, Pointable d) =>
	Device.D ds -> CreateInfo n objs ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . B s objs -> IO a) -> IO a
create (Device.D dvc) ci macc macd f = bracket
	(M.create dvc (createInfoToMiddle ci) macc)
	(\b -> M.destroy dvc b macd)
	(f . B (createInfoLengths ci) . (\(M.B b) -> b))
