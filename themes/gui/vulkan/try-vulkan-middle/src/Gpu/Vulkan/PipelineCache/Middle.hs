{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache.Middle (

	-- * CREATE AND DESTROY

	create, destroy, P, CreateInfo(..), getData, Data(..),

	) where

import Gpu.Vulkan.PipelineCache.Middle.Internal
