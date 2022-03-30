{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Subpass where

import Vulkan.Subpass.Enum

import qualified Vulkan.Pipeline.Enum as Pipeline

data Description = Description {
	descriptionFlags :: DescriptionFlags,
	descriptionPIpelineBindPoint :: Pipeline.BindPoint
	-- TODO
	}
	deriving Show
