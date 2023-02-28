{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanRenderPassEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "RenderPass.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkRenderPassCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	"type CreateFlags = CreateFlagBits"
