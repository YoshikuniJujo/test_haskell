{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.ImageBuffer.Kind where

import GHC.TypeLits
import Data.Kind.Object

import qualified Gpu.Vulkan.TypeEnum as T

data ImageBuffer = Image Symbol T.Format | Buffer [Object]
