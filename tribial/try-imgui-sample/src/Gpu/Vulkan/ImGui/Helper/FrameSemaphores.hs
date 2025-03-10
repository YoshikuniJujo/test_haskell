{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.FrameSemaphores where

import Gpu.Vulkan.Semaphore.Internal qualified as Vk.Smph

import Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Middle qualified as M

data FC sias srcs = FC {
	fCImageAcquiredSemaphore :: Vk.Smph.S sias,
	fCRenderCompleteSemaphore :: Vk.Smph.S srcs }
	deriving Show

fCToMiddle :: FC sias srcs -> M.FC
fCToMiddle FC {
	fCImageAcquiredSemaphore = Vk.Smph.S ias,
	fCRenderCompleteSemaphore = Vk.Smph.S rcs } = M.FC {
	M.fCImageAcquiredSemaphore = ias,
	M.fCRenderCompleteSemaphore = rcs }
