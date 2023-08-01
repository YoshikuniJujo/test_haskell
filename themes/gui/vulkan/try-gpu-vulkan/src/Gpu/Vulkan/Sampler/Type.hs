{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sampler.Type where

import qualified Gpu.Vulkan.Sampler.Middle as M

newtype S s = S { sToMiddle :: M.S } deriving Show
