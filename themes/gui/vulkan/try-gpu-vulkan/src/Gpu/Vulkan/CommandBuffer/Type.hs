{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Type where

import Data.Kind

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as DscStLyt
import Gpu.Vulkan.CommandBuffer.Middle qualified as M

import Gpu.Vulkan.VertexInput qualified as VertexInput

newtype C s = C { unC :: M.C }

newtype GBinded s (vibs :: [(Type, VertexInput.Rate)])
	(largs :: (Type, [(Type, [DscStLyt.BindingType])], [Type])) =
	GBinded { unGBinded :: M.C }

newtype CBinded s
	(largs :: (Type, [(Type, [DscStLyt.BindingType])], [Type])) =
	CBinded { unCBinded :: M.C }
