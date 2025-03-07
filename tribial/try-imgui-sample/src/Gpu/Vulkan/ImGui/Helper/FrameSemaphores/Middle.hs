{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Middle (

	-- * DATA TYPE

	FC(..),

	-- * MUTABLE

	C.FCIO, fCFreeze, fCThaw,

	-- * CXX TO/FROM MUTABLE

	C.F(..), C.FTag, fromCxx, toCxx,

	-- * INTERNAL

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

fCFreeze :: C.FCIO -> IO FC
fCFreeze = (fCFromCore <$>) . C.fCFreeze

fCThaw :: FC -> IO C.FCIO
fCThaw = C.fCThaw . fCToCore

fromCxx :: C.F -> IO C.FCIO
fromCxx = C.toC

toCxx :: C.FCIO -> (C.F -> IO a) -> IO a
toCxx = C.fromC
