{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Type where

import Data.Kind
import Data.Kind.Object qualified as KObj
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.IORef
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.DescriptorSet.Middle qualified as M
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout

data D s (slbts :: (Type, [Layout.BindingType])) = D
	(IORef (HeteroParList.PL2 KObj.NObjectLength
		(Layout.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))))
	M.D
