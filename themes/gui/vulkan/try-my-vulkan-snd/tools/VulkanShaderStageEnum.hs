{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanShaderStageEnum where

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h" "Shader.Stage.Enum"
	["Data.Word", "Data.Bits"]
	[("FlagBits", "VkShaderStageFlagBits",
		["Show", "Eq", "Storable", "Bits"])]
	"type Flags = FlagBits"
