{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer.Type where

import {-# SOURCE #-} qualified Vulkan.CommandBuffer.Middle as M

newtype C s vs = C { unC :: M.C vs } deriving Show
