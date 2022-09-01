{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.ImageBuffer where

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Image as Image
import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Device.Memory.ImageBuffer.Kind as K
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

data ImageBuffer sib (ib :: K.ImageBuffer) where
	Image :: Image.INew si fmt -> ImageBuffer si ('K.Image fmt)
	Buffer :: Buffer.B sb objs -> ImageBuffer sb ('K.Buffer objs)

getMemoryRequirements :: Device.D sd -> ImageBuffer sib fos -> IO Memory.M.Requirements
getMemoryRequirements (Device.D dvc) (Buffer (Buffer.B _ b)) =
	Buffer.M.getMemoryRequirements dvc (Buffer.M.B b)
-- getMemoryRequirements (Device.D dvc) (Image ...
