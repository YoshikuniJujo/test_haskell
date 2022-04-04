{-# OPTIONS_gHC -Wall -fno-warn-tabs #-}

module VulkanCommandBufferEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "CommandBuffer.Enum" ["Data.Word"] [
	(	[],
		(	"Level", "VkCommandBufferLevel",
			["Show", "Eq", "Storable"] ) ) ]
	""
