{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h" "Enum"
		["Foreign.Ptr", "Data.Bits", "Data.Word"] [
	("SystemAllocationScope","VkSystemAllocationScope",
		["Show", "Eq", "Storable"]),
	("InternalAllocationType", "VkInternalAllocationType",
		["Show", "Eq", "Storable"]),
	("ObjectType", "VkObjectType", ["Show", "Eq", "Storable"]),
	("QueueFlagBits", "VkQueueFlagBits",
		["Show", "Eq", "Storable", "Bits"]),
	("Format", "VkFormat", ["Show", "Eq", "Storable"]),
	("SharingMode", "VkSharingMode", ["Show", "Eq", "Storable"]),
	("PrimitiveTopology", "VkPrimitiveTopology", ["Show", "Eq", "Storable"]),
	("PolygonMode", "VkPolygonMode", ["Show", "Eq", "Storable"]),
	("FrontFace", "VkFrontFace", ["Show", "Eq", "Storable"]),
	("CompareOp", "VkCompareOp", ["Show", "Eq", "Storable"]),
	("StencilOp", "VkStencilOp", ["Show", "Eq", "Storable"]),
	("BlendFactor", "VkBlendFactor", ["Show", "Eq", "Storable"]),
	("BlendOp", "VkBlendOp", ["Show", "Eq", "Storable"]),
	("LogicOp", "VkLogicOp", ["Show", "Eq", "Storable"]),
	("DynamicState", "VkDynamicState", ["Show", "Eq", "Storable"])
	] [nowdoc|
type QueueFlags = QueueFlagBits
type PtrDynamicState = Ptr DynamicState|]
