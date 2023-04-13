{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPipelineCacheEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "PipelineCache.Enum"
		["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkPipelineCacheCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	"type CreateFlags = CreateFlagBits"
