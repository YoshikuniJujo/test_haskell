{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h" "Enum"
	["Foreign.Ptr", "Data.Bits", "Data.Word"] ((([] ,) <$> noZeros) ++ zeros)
	[nowdoc|
type QueueFlags = QueueFlagBits
type PtrDynamicState = Ptr DynamicState
type AccessFlags = AccessFlagBits
type DependencyFlags = DependencyFlagBits
type QueryControlFlags = QueryControlFlagBits|]

noZeros :: [(HaskellName, CName, [DerivName])]
noZeros = [
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
	("DynamicState", "VkDynamicState", ["Show", "Eq", "Storable"]),
	("DescriptorType", "VkDescriptorType", ["Show", "Eq", "Storable"])
	]

zeros :: [([(String, Const)], (HaskellName, CName, [DerivName]))]
zeros = [
	(	[("AccessFlagsZero", Int 0)],
		(	"AccessFlagBits", "VkAccessFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[("DependencyFlagsZero", Int 0)],
		(	"DependencyFlagBits", "VkDependencyFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[("QueryControlFlagsZero", Int 0)],
		(	"QueryControlFlagBits", "VkQueryControlFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[("QueryPipelineStatisticFlagsZero", Int 0)],
		(	"QueryPipelineStatisticFlagBits",
			"VkQueryPipelineStatisticFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	]
