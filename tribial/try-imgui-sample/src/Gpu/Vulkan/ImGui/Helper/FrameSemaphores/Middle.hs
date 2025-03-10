{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Middle (

	-- * DATA TYPE

	FC(..),

	-- * TO/FROM CORE

	fCToCore, fCFromCore

	) where

import Gpu.Vulkan.Semaphore.Middle.Internal qualified as Vk.Smph

import Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Core qualified as C

data FC = FC {
	fCImageAcquiredSemaphore :: Vk.Smph.S,
	fCRenderCompleteSemaphore :: Vk.Smph.S }
	deriving Show

fCToCore :: FC -> C.FC
fCToCore FC {
	fCImageAcquiredSemaphore = Vk.Smph.S ias,
	fCRenderCompleteSemaphore = Vk.Smph.S rcs } = C.FC {
	C.fCImageAcquiredSemaphore = ias,
	C.fCRenderCompleteSemaphore = rcs }

fCFromCore :: C.FC -> FC
fCFromCore C.FC {
	C.fCImageAcquiredSemaphore = ias,
	C.fCRenderCompleteSemaphore = rcs } = FC {
	fCImageAcquiredSemaphore = Vk.Smph.S ias,
	fCRenderCompleteSemaphore = Vk.Smph.S rcs }
