{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Type where

import {-# SOURCE #-} qualified Gpu.Vulkan.CommandBuffer.Middle.Internal as M

newtype C s vs = C { unC :: M.C vs }
