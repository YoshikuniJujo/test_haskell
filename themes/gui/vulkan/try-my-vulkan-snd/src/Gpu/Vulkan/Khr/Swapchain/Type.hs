{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Type where

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

newtype S ss (fmt :: T.Format) = S M.S deriving Show
