{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass.Type where

import qualified Vulkan.RenderPass.Middle as M

newtype R s = R M.R deriving Show
