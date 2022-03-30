{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPipelineEnum where

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h" "Pipeline.Enum"
		["Data.Word"] [
	(	[],
		(	"BindPoint", "VkPipelineBindPoint",
			["Show", "Storable"] ) ),
	(	[("StageFlagsZero", Int 0)],
		(	"StageFlagBits", "VkPipelineStageFlagBits",
			["Show", "Storable"] ) )
	]
	"type StageFlags = StageFlagBits"
