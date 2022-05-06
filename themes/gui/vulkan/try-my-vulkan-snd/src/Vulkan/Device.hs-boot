{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device where

import Data.Word

newtype Size = Size Word64

instance Show Size
instance Num Size
