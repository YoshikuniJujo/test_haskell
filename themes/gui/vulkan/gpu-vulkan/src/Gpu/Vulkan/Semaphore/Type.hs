{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Type where

import Gpu.Vulkan.Semaphore.Middle qualified as M

newtype S s = S M.S deriving Show
