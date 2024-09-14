{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Type (D(..)) where

import Data.Kind
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.IORef
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.Object.Base qualified as KObj
import Gpu.Vulkan.DescriptorSet.Middle qualified as M
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Lyt

data D s (slbts :: (Type, [Lyt.BindingType])) = D
	(IORef (HeteroParList.PL2 KObj.Length
		(Lyt.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 slbts))))
	M.D
