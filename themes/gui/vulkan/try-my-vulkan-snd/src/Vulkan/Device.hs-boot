{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device where

import Data.Word

import qualified Vulkan.Device.Core as C

newtype Size = Size Word64

instance Show Size
instance Num Size

newtype D = D C.D
instance Show D
