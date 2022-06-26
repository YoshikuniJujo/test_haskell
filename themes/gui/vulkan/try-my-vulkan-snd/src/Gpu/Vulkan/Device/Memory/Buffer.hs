{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.Buffer where

import Data.Kind.Object
import Data.HeteroList

import Gpu.Vulkan.Device.Memory.Buffer.Types
import Gpu.Vulkan.Memory

import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Device.Core as Device.C

data M s (objss :: [[Object]]) =
	M (HeteroVarList Form objss) Device.C.Memory

deriving instance Show (HeteroVarList Form objss) => Show (M s objss)

deriving instance Show (HeteroVarList ObjectLength objs) => Show (Form objs)

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoMemoryTypeIndex :: TypeIndex }
	deriving Show
