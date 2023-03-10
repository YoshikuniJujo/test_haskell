{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Kind where

import GHC.TypeLits
import Gpu.Vulkan.Object

import qualified Gpu.Vulkan.TypeEnum as T

data ImageBuffer = Image Symbol T.Format | Buffer Symbol [Object]
