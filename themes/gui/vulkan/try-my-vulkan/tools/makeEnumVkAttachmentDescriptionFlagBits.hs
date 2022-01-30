{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import MakeEnum

main :: IO ()
main = makeEnum''
	"/usr/include/vulkan/vulkan_core.h" ["Data.Bits"]
	"AttachmentDescriptionFlagBits" "VkAttachmentDescriptionFlagBits"
	[("AttachmentDescriptionFlagsZero", Int 0)]
	["Show", "Eq", "Storable", "Bits"]
	$ "type AttachmentDescriptionFlags = AttachmentDescriptionFlagBits"
