{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanFormatEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Format.Enum" ["Data.Word"] [
	(	[],
		("F", "VkFormat", ["Show", "Eq", "Storable"]) )
	]
	""
