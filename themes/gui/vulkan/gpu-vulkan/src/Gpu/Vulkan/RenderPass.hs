{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass (

	-- * CREATE

	create, R, CreateInfo(..),

	-- ** Group

	group, Group, create', unsafeDestroy, lookup,

	-- * BEGIN INFO

	BeginInfo(..),

	-- * ENUM

	module Gpu.Vulkan.RenderPass.Enum

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.RenderPass.Internal
import Gpu.Vulkan.RenderPass.Enum
