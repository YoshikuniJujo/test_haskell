{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = do
	createRaw vulkanCore ["ShaderStageFlagsZero"] "VkShaderStageFlagBits"
	createRaw vulkanCore [] "VkFormat"

noZeros :: [(HaskellName, CName, [DerivName])]
noZeros = [
	("StructureType", "VkStructureType", ["Show", "Eq", "Storable"]),
	("SystemAllocationScope","VkSystemAllocationScope",
		["Show", "Eq", "Storable"]),
	("InternalAllocationType", "VkInternalAllocationType",
		["Show", "Eq", "Storable"]),
	("ObjectType", "VkObjectType", ["Show", "Eq", "Storable"]),
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
	("IndexType", "VkIndexType", ["Show", "Eq", "Storable"]),
	("Filter", "VkFilter", ["Show", "Eq", "Storable"]),
	("BorderColor", "VkBorderColor", ["Show", "Eq", "Storable"]),
	("Format", "VkFormat", ["Show", "Eq", "Storable"])
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
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[("CullModeFlagsZero", Int 0)],
		(	"CullModeFlagBits", "VkCullModeFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[("ShaderStageFlagsZero", Int 0)],
		(	"ShaderStageFlagBits", "VkShaderStageFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[("FormatFeatureFlagsZero", Int 0)],
		(	"FormatFeatureFlagBits", "VkFormatFeatureFlagBits",
			["Show", "Eq", "Storable", "Bits", "FiniteBits"] ) )
	]
