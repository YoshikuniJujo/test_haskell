{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Type where

import qualified Gpu.Vulkan.RenderPass.Middle as M

newtype R s = R M.R deriving Show
