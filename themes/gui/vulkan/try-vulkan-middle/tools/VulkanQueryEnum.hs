{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanQueryEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Query.Enum" ["Data.Bits", "Data.Word"] [
	(	[],
		(	"PipelineStatisticFlagBits",
				"VkQueryPipelineStatisticFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[],
		(	"ControlFlagBits", "VkQueryControlFlagBits",
			["Show", "Eq", "Storable", "Bits"] )),
	(	[],
		(	"ResultFlagBits", "VkQueryResultFlagBits",
			["Show", "Eq", "Storable", "Bits"] )),
	(	[],
		(	"Type", "VkQueryType",
			["Show", "Eq", "Storable", "Bits"] )) ] $
	"type PipelineStatisticFlags = PipelineStatisticFlagBits\n" ++
	"type ControlFlags = ControlFlagBits\n" ++
	"type ResultFlags = ResultFlagBits"
