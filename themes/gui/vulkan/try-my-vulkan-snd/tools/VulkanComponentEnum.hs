{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanComponentEnum where

import MakeEnum

make :: IO ()
make = createFileWithDefault vulkanCore "Component.Enum" ["Data.Word"] [
	(	Just "SwizzleIdentity", [],
		("Swizzle", "VkComponentSwizzle", ["Show", "Eq", "Storable"]) )
	] []
