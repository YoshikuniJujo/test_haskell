{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass.Middle where

import qualified Vulkan.RenderPass.Core as C

newtype R = R C.R

instance Show R
