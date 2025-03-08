{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Core (

	-- * DATA TYPE

	FC, PtrFC, pattern FC,
	fCImageAcquiredSemaphore, fCRenderCompleteSemaphore,

	-- * MUTABLE

	FCPrim, FCST, FCIO,

	fCFreeze, fCThaw, fCCopy,

	-- * CXX TO/FROM C

	F(..), FTag, toC, fromC

	) where

#include "imgui_impl_vulkan_helper_c.h"

import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.Struct
import Control.Monad.Primitive
import Data.Word

import Gpu.Vulkan.Semaphore.Core qualified as Vk.Smph

struct "FC" #{size ImGui_ImplVulkanH_FrameSemaphores_C}
	#{alignment ImGui_ImplVulkanH_FrameSemaphores_C} [
	("ImageAcquiredSemaphore", ''Vk.Smph.S,
		[| #{peek ImGui_ImplVulkanH_FrameSemaphores_C,
			ImageAcquiredSemaphore} |],
		[| #{poke ImGui_ImplVulkanH_FrameSemaphores_C,
			ImageAcquiredSemaphore} |]),
	("RenderCompleteSemaphore", ''Vk.Smph.S,
		[| #{peek ImGui_ImplVulkanH_FrameSemaphores_C,
			RenderCompleteSemaphore} |],
		[| #{poke ImGui_ImplVulkanH_FrameSemaphores_C,
			RenderCompleteSemaphore} |])
	]
	[''Show, ''Storable]

type PtrFC = Ptr FC

foreign import ccall "copyImguiImplVulkanHFramesemaphoresC"
	cxx_copyImguiImplVulkanHFramesemaphoresC :: Ptr FC -> IO (Ptr FC)

foreign import ccall "freeImguiImplVulkanHFramesemaphoresC"
	cxx_freeImguiImplVulkanHFramesemaphoresC :: Ptr FC -> IO ()

structPrim "FC"
	'cxx_copyImguiImplVulkanHFramesemaphoresC
	'cxx_freeImguiImplVulkanHFramesemaphoresC [''Show]

toC :: PrimMonad m => F -> m (FCPrim (PrimState m))
toC (F pf) = unsafeIOToPrim do
	pfc <- malloc
	cxx_imguiImplVulkanHFramesemaphoresToC pf pfc
	FCPrim <$> newForeignPtr pfc (free pfc)

fromC :: FCPrim s -> (F -> IO a) -> IO a
fromC (FCPrim ffc) a =
	allocaBytesAligned
		(fromIntegral cxx_sizeofImguiImplVulkanHFramesemaphores)
		(fromIntegral cxx_alignofImguiImplVulkanHFramesemaphores) \pf ->
	withForeignPtr ffc \pfc -> do
	cxx_imguiImplVulkanHFramesemaphoresFromC pfc pf
	a $ F pf

newtype F = F (Ptr FTag) deriving Show
data FTag

foreign import ccall "sizeofImguiImplVulkanHFramesemaphores"
	cxx_sizeofImguiImplVulkanHFramesemaphores :: #{type size_t}

foreign import ccall "alignofImguiImplVulkanHFramesemaphores"
	cxx_alignofImguiImplVulkanHFramesemaphores :: #{type size_t}

foreign import ccall "imguiImplVulkanHFramesemaphoresFromC"
	cxx_imguiImplVulkanHFramesemaphoresFromC :: Ptr FC -> Ptr FTag -> IO ()

foreign import ccall "imguiImplVulkanHFramesemaphoresToC"
	cxx_imguiImplVulkanHFramesemaphoresToC :: Ptr FTag -> Ptr FC -> IO ()
