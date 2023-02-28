{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle.Internal (D(..), Size(..)) where

import Data.Word

import qualified Gpu.Vulkan.Device.Core as C

newtype Size = Size Word64

instance Show Size
instance Num Size

newtype D = D C.D
instance Show D
