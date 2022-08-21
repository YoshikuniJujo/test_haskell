{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Type where

import qualified Gpu.Vulkan.Image.Middle as M

newtype I s = I M.I

newtype Binded si sm = Binded M.I
