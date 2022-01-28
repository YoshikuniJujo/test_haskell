{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import MakeEnum

main :: IO ()
main = makeEnum'
	"/usr/include/vulkan/vulkan_core.h" ["Data.Bits"]
	"SamplerCreateFlagBits" "VkSamplerCreateFlagBits"
	["Show", "Eq", "Storable", "Bits"]
	"type SamplerCreateFlags = SamplerCreateFlagBits"
