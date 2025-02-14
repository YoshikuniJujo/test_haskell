{-# LANGUAGE QuasiQuotes #-}

module VulkanKhrSurfaceEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFileWithDefault vulkanCore "Khr.Surface.Enum"
		["Data.Bits", "Data.Word"] [
	(	Just "TransformFlagsZero", [("TransformFlagsZero", Int 0)],
		(	"TransformFlagBits", "VkSurfaceTransformFlagBitsKHR",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	Just "CompositeAlphaFlagsZero", [("CompositeAlphaFlagsZero", Int 0)],
		(	"CompositeAlphaFlagBits", "VkCompositeAlphaFlagBitsKHR",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	Nothing, [],
		(	"ColorSpace", "VkColorSpaceKHR",
			["Show", "Eq", "Storable"] ) ),
	(	Nothing, [],
		(	"PresentMode", "VkPresentModeKHR",
			["Show", "Eq", "Storable"] ) )
	]
	[nowdoc|
type TransformFlags = TransformFlagBits
type CompositeAlphaFlags = CompositeAlphaFlagBits|]
