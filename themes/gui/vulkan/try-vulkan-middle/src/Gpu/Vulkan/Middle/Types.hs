{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Middle.Types where

import Data.Word

newtype Size = Size Word64 deriving Show
