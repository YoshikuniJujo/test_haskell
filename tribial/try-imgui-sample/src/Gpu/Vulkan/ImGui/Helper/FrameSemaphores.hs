{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.FrameSemaphores where

import Gpu.Vulkan.Semaphore.Internal qualified as Vk.Smph

data FC sias srcs = FC {
	fCImageAcquiredSemaphore :: Vk.Smph.S sias,
	fCRenderCompleteSemaphore :: Vk.Smph.S srcs }
	deriving Show
