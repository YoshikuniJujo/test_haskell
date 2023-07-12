{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Type where

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

newtype S (fmt :: T.Format) ss = S M.S deriving Show
