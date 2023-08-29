{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Middle (

	-- * CREATE AND DESTROY

	create, recreate, recreate', destroy, I, CreateInfo(..),

	-- ** Manage Multiple Image Views

	Manager, manage, create', destroy', lookup

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.ImageView.Middle.Internal
