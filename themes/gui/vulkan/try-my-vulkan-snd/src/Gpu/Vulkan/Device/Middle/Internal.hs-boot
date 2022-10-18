{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle.Internal (
	D(..),

	Memory(..),

	Size(..), MemoryImage(..), MemoryList
	) where

import Data.IORef
import Data.Word

import qualified Gpu.Vulkan.Device.Core as C
import qualified Gpu.Vulkan.Memory.Core as Memory

newtype Size = Size Word64

instance Show Size
instance Num Size

newtype D = D C.D
instance Show D

newtype Memory = Memory (IORef Memory.M)

data MemoryImage = MemoryImage Memory.M
instance Show MemoryImage

type role MemoryList phantom
data MemoryList v
instance Show (MemoryList v)
