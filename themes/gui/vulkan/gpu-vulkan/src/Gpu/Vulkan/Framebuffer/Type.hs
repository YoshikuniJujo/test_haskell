{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Type where

import qualified Gpu.Vulkan.Framebuffer.Middle as M

newtype F s = F M.F
