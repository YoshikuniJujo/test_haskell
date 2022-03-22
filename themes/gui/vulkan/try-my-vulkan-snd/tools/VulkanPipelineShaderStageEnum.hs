{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPipelineShaderStageEnum where

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h" "Pipeline.ShaderStage.Enum"
		["Data.Word", "Data.Bits"] [
	(	[("CreateFlagsZero", Int 0)],
		("CreateFlagBits", "VkPipelineShaderStageCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"]) )
	]
	"type CreateFlags = CreateFlagBits"
