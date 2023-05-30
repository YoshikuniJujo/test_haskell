{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Core (

	-- * CREATE AND DESTROY

	create, destroy, I, PtrI, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoImage, createInfoViewType, createInfoFormat,
	createInfoComponents, createInfoSubresourceRange,

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.Image.Core as Image
import qualified Gpu.Vulkan.Component.Core as Component

#include <vulkan/vulkan.h>

data ITag
type I = Ptr ITag
type PtrI = Ptr I

strType :: #{type VkStructureType}
strType = #{const VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO}

struct "CreateInfo" #{size VkImageViewCreateInfo}
		#{alignment VkImageViewCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkImageViewCreateInfo, sType} p strType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkImageViewCreateInfo, pNext} |],
		[| #{poke VkImageViewCreateInfo, pNext} |]),
	("flags", ''#{type VkImageViewCreateFlags},
		[| #{peek VkImageViewCreateInfo, flags} |],
		[| #{poke VkImageViewCreateInfo, flags} |]),
	("image", ''Image.I,
		[| #{peek VkImageViewCreateInfo, image} |],
		[| #{poke VkImageViewCreateInfo, image} |]),
	("viewType", ''#{type VkImageViewType},
		[| #{peek VkImageViewCreateInfo, viewType} |],
		[| #{poke VkImageViewCreateInfo, viewType} |]),
	("format", ''#{type VkFormat},
		[| #{peek VkImageViewCreateInfo, format} |],
		[| #{poke VkImageViewCreateInfo, format} |]),
	("components", ''Component.Mapping,
		[| #{peek VkImageViewCreateInfo, components} |],
		[| #{poke VkImageViewCreateInfo, components} |]),
	("subresourceRange", ''Image.SubresourceRange,
		[| #{peek VkImageViewCreateInfo, subresourceRange} |],
		[| #{poke VkImageViewCreateInfo, subresourceRange} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateImageView" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr I ->
	IO #{type VkResult}

foreign import ccall "vkDestroyImageView" destroy ::
	Device.D -> I -> Ptr AllocationCallbacks.A -> IO ()
