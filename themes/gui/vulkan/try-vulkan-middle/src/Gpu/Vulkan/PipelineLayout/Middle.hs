{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineLayout.Middle (

	-- * CREATE AND DESTROY

	create, destroy, P, CreateInfo(..), CreateFlags ) where

import Gpu.Vulkan.PipelineLayout.Middle.Internal
