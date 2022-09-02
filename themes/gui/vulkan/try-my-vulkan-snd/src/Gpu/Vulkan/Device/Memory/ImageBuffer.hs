{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.ImageBuffer where

import Data.HeteroList

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Image.Middle as Image.M
import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Device.Memory.ImageBuffer.Kind as K
import qualified Gpu.Vulkan.Device.Memory.Buffer as Device.Memory.Buffer
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

data ImageBuffer sib (ib :: K.ImageBuffer) where
	Image :: Image.INew si fmt -> ImageBuffer si ('K.Image fmt)
	Buffer :: Buffer.B sb objs -> ImageBuffer sb ('K.Buffer objs)

getMemoryRequirements ::
	Device.D sd -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc (Buffer.M.B b)
getMemoryRequirements (Device.D dvc) (Image (Image.INew i)) =
	Image.M.getMemoryRequirements dvc i

getMemoryRequirements' ::
	Device.D sd -> V2 ImageBuffer sibfos -> IO Memory.M.Requirements
getMemoryRequirements' dvc (V2 bi) = getMemoryRequirements dvc bi

getMemoryRequirementsList :: Device.D sd ->
	HeteroVarList (V2 ImageBuffer) sibfoss -> IO [Memory.M.Requirements]
getMemoryRequirementsList dvc bis =
	heteroVarListToListM (getMemoryRequirements' dvc) bis

allocateInfoToMiddle ::
	Device.D sd -> HeteroVarList (V2 ImageBuffer) sibfoss ->
	Device.Memory.Buffer.AllocateInfo n -> IO (Memory.M.AllocateInfo n)
allocateInfoToMiddle dvc ibs Device.Memory.Buffer.AllocateInfo {
	Device.Memory.Buffer.allocateInfoNext = mnxt,
	Device.Memory.Buffer.allocateInfoMemoryTypeIndex = mti } = do
	reqss <- getMemoryRequirementsList dvc ibs
	pure Memory.M.AllocateInfo {
		Memory.M.allocateInfoNext = mnxt,
		Memory.M.allocateInfoAllocationSize =
			memoryRequirementsListToSize 0 reqss,
		Memory.M.allocateInfoMemoryTypeIndex = mti }

memoryRequirementsListToSize ::
	Device.M.Size -> [Memory.M.Requirements] -> Device.M.Size
memoryRequirementsListToSize sz0 [] = sz0
memoryRequirementsListToSize sz0 (reqs : reqss) =
	memoryRequirementsListToSize
		(((sz0 - 1) `div` algn + 1) * algn + sz) reqss
	where
	sz = Memory.M.requirementsSize reqs
	algn = Memory.M.requirementsAlignment reqs
