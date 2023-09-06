{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import MakeEnum

main :: IO ()
main = makeEnum''
	"/usr/include/vulkan/vulkan_core.h" ["Data.Bits", "Foreign.Ptr"]
	"PipelineStageFlagBits" "VkPipelineStageFlagBits"
	[("PipelineStageFlagsZero", Int 0)]
	["Show", "Eq", "Storable", "Bits"]
	$	"type PipelineStageFlags = PipelineStageFlagBits\n" ++
		"type PtrPipelineStageFlags = Ptr PipelineStageFlags"
