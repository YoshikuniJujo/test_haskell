{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Middle (

	-- * CREATE AND DESTROY

	create, recreate, recreate', destroy, I, CreateInfo(..),

	-- ** Manage Destruction

	manage, create', Manager

	) where

import Gpu.Vulkan.ImageView.Middle.Internal
