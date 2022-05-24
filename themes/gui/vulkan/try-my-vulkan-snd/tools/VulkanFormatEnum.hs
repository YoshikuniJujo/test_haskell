{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanFormatEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Format.Enum" ["Data.Bits", "Data.Word"] [
	(	[],
		("F", "VkFormat", ["Show", "Eq", "Storable"]) ),
	(	[("FeatureFlagsZero", Int 0)],
		(	"FeatureFlagBits", "VkFormatFeatureFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	]
	[nowdoc|
type FeatureFlags = FeatureFlagBits|]
