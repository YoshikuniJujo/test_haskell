{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Middle.Internal where

import qualified Gpu.Vulkan.RenderPass.Core as C

newtype R = R C.R

instance Show R
