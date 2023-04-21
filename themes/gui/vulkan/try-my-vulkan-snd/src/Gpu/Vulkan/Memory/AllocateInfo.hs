{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.AllocateInfo where

import Data.TypeLevel.Maybe qualified as TMaybe

import Gpu.Vulkan.Memory.Middle qualified as M

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: TMaybe.M n,
	allocateInfoMemoryTypeIndex :: M.TypeIndex }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn)
deriving instance Eq (TMaybe.M mn) => Eq (AllocateInfo mn)
