{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Helper.Window.Core (

	-- * DATA TYPE

	WC, wCZero, pattern WC,
	wCWidth, wCHeight,
	wCSwapchain, wCSurface, wCSurfaceFormat, wCPresentMode, wCRenderPass,
	wCPipeline, wCUseDynamicRendering, wCClearEnable, wCClearValue,
	wCFrameIndex, wCImageCount, wCSemaphoreCount, wCSemaphoreIndex,
	wCFramec, wCPFrames, wCFrameSemaphorec, wCPFrameSemaphores,

	-- * CXX TO/FROM C

	W(..), WTag, toC, copyFromC, allocaW

	) where

import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable (Storable(..))
import Foreign.C.Struct
import Data.Word
import Data.Int
import System.IO.Unsafe

import Gpu.Vulkan.Core qualified as Vk
import Gpu.Vulkan.Pipeline.Core qualified as Vk.Ppl
import Gpu.Vulkan.RenderPass.Core qualified as Vk.RndrPss

import Gpu.Vulkan.Khr.Swapchain.Core qualified as Vk.Swpch
import Gpu.Vulkan.Khr.Surface.Core qualified as Vk.Sfc

import Gpu.Vulkan.ImGui.Helper.Frame.Core qualified as Vk.ImGui.HFrame
import Gpu.Vulkan.ImGui.Helper.FrameSemaphores.Core
	qualified as Vk.ImGui.HFrameSemaphores

#include "imgui_impl_vulkan_helper_c.h"

type ListWord32 = [Word32]

struct "WC" #{size ImGui_ImplVulkanH_Window_C}
	#{alignment ImGui_ImplVulkanH_Window_C} [
	("Width", ''#{type int},
		[| #{peek ImGui_ImplVulkanH_Window_C, Width} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Width} |]),
	("Height", ''#{type int},
		[| #{peek ImGui_ImplVulkanH_Window_C, Height} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Height} |]),
	("Swapchain", ''Vk.Swpch.S,
		[| #{peek ImGui_ImplVulkanH_Window_C, Swapchain} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Swapchain} |]),
	("Surface", ''Vk.Sfc.S,
		[| #{peek ImGui_ImplVulkanH_Window_C, Surface} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Surface} |]),
	("SurfaceFormat", ''Vk.Sfc.Format,
		[| #{peek ImGui_ImplVulkanH_Window_C, SurfaceFormat} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, SurfaceFormat} |]),
	("PresentMode", ''#{type VkPresentModeKHR},
		[| #{peek ImGui_ImplVulkanH_Window_C, PresentMode} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, PresentMode} |]),
	("RenderPass", ''Vk.RndrPss.R,
		[| #{peek ImGui_ImplVulkanH_Window_C, RenderPass} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, RenderPass} |]),
	("Pipeline", ''Vk.Ppl.P,
		[| #{peek ImGui_ImplVulkanH_Window_C, Pipeline} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Pipeline} |]),
	("UseDynamicRendering", ''#{type bool},
		[| #{peek ImGui_ImplVulkanH_Window_C, UseDynamicRendering} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, UseDynamicRendering} |]),
	("ClearEnable", ''#{type bool},
		[| #{peek ImGui_ImplVulkanH_Window_C, ClearEnable} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, ClearEnable} |]),
	("ClearValue", ''ListWord32,
		[| peekArray 4 . #{ptr ImGui_ImplVulkanH_Window_C, ClearValue} |],
		[| \p -> pokeArray (#{ptr ImGui_ImplVulkanH_Window_C, ClearValue} p) . take 4 |]),
	("FrameIndex", ''#{type uint32_t},
		[| #{peek ImGui_ImplVulkanH_Window_C, FrameIndex} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, FrameIndex} |]),
	("ImageCount", ''#{type uint32_t},
		[| #{peek ImGui_ImplVulkanH_Window_C, ImageCount} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, ImageCount} |]),
	("SemaphoreCount", ''#{type uint32_t},
		[| #{peek ImGui_ImplVulkanH_Window_C, SemaphoreCount} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, SemaphoreCount} |]),
	("SemaphoreIndex", ''#{type uint32_t},
		[| #{peek ImGui_ImplVulkanH_Window_C, SemaphoreIndex} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, SemaphoreIndex} |]),
	("Framec", ''#{type int},
		[| #{peek ImGui_ImplVulkanH_Window_C, Framec} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, Framec} |]),
	("pFrames", ''Vk.ImGui.HFrame.PtrFC,
		[| #{peek ImGui_ImplVulkanH_Window_C, pFrames} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, pFrames} |]),
	("FrameSemaphorec", ''#{type int},
		[| #{peek ImGui_ImplVulkanH_Window_C, FrameSemaphorec} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, FrameSemaphorec} |]),
	("pFrameSemaphores", ''Vk.ImGui.HFrameSemaphores.PtrFC,
		[| #{peek ImGui_ImplVulkanH_Window_C, pFrameSemaphores} |],
		[| #{poke ImGui_ImplVulkanH_Window_C, pFrameSemaphores} |]) ]
	[''Show, ''Storable]

wCZero :: WC
wCZero = unsafePerformIO do
	p <- calloc
	WC_ <$> newForeignPtr p (free p)

toC :: W -> IO WC
toC (W pw) = do
	pwc <- malloc
	cxx_imguiImplVulkanHWindowToC pw pwc
	WC_ <$> newForeignPtr pwc (free pwc)

copyFromC :: WC -> W -> IO ()
copyFromC (WC_ fwc) (W pw) = withForeignPtr fwc \pwc ->
	cxx_imguiImplVulkanHWindowFromC pwc pw

newtype W = W (Ptr WTag) deriving Show
data WTag

allocaW :: (W -> IO a) -> IO a
allocaW a = allocaBytesAligned sizeOfW alignOfW \p ->do
	let	w = W p
		z = wCZero
	copyFromC z { wCClearEnable = 1 } w
	a w

sizeOfW, alignOfW :: Integral n => n
sizeOfW = fromIntegral cxx_sizeofImguiImplVulkanHWindow
alignOfW = fromIntegral cxx_alignofImguiImplVulkanHWindow

foreign import ccall "sizeofImguiImplVulkanHWindow"
	cxx_sizeofImguiImplVulkanHWindow :: #{type size_t}

foreign import ccall "alignofImguiImplVulkanHWindow"
	cxx_alignofImguiImplVulkanHWindow :: #{type size_t}

foreign import ccall "imguiImplVulkanHWindowFromC"
	cxx_imguiImplVulkanHWindowFromC :: Ptr WC -> Ptr WTag -> IO ()

foreign import ccall "imguiImplVulkanHWindowToC"
	cxx_imguiImplVulkanHWindowToC :: Ptr WTag -> Ptr WC -> IO ()
