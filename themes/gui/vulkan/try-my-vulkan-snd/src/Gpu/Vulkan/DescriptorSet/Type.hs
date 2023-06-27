{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Type where

import Data.Kind.Object qualified as KObj
import Data.IORef
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.DescriptorSet.TypeLevel.Common
import Gpu.Vulkan.DescriptorSet.Middle qualified as M

data D s (slbts :: LayoutArg) = D
	(IORef (HeteroParList.PL2 KObj.ObjectLength
		(LayoutArgOnlyDynamics slbts)))
	M.D
