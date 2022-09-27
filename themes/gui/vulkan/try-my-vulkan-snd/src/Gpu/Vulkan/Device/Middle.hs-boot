{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle where

import Data.IORef
import Data.Word

import qualified Gpu.Vulkan.Device.Core as C

newtype Size = Size Word64

instance Show Size
instance Num Size

newtype D = D C.D
instance Show D

newtype Memory = Memory (IORef C.Memory)

data MemoryImage = MemoryImage C.Memory
instance Show MemoryImage

type role MemoryList phantom
data MemoryList v
instance Show (MemoryList v)
