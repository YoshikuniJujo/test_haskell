{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Middle (

	-- * CREATE AND DESTROY

	create, recreate, destroy, I, CreateInfo(..) ) where

import Gpu.Vulkan.ImageView.Middle.Internal
