{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.ImageBuffer where

import qualified Gpu.Vulkan.Image as Image
import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Device.Memory.ImageBuffer.Kind as K

data ImageBuffer (ib :: K.ImageBuffer) where
	Image :: Image.INew si fmt -> ImageBuffer ('K.Image fmt)
	Buffer :: Buffer.B sb objs -> ImageBuffer ('K.Buffer objs)
