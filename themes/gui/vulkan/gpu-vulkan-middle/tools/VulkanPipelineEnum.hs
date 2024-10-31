{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPipelineEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFileWithDefault "/usr/include/vulkan/vulkan_core.h" "Pipeline.Enum"
		["Data.Bits", "Data.Word"] [
	(	Nothing, [],
		(	"BindPoint", "VkPipelineBindPoint",
			["Show", "Storable"] ) ),
	(	Just "StageFlagsZero", [("StageFlagsZero", Int 0)],
		(	"StageFlagBits", "VkPipelineStageFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	Just "CreateFlagsZero", [("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkPipelineCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	] [nowdoc|
type StageFlags = StageFlagBits
type CreateFlags = CreateFlagBits|]
