{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Set where

import Data.Word

import qualified Vulkan.Descriptor.Pool as Descriptor.Pool
import qualified Vulkan.Descriptor.Set.Layout as Layout
import qualified Vulkan.Descriptor.Set.Middle as M

data AllocateInfo n sp sl = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoDescriptorSetCountOrSetLayouts :: Either Word32 [Layout.L sl] }
	deriving Show
