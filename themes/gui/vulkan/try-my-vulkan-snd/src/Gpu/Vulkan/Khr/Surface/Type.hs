{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Type where

import Gpu.Vulkan.Khr.Surface.Middle as M

newtype S ss = S M.S deriving Show
