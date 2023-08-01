{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Type (G(..)) where

import GHC.TypeNats
import Data.Kind

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as DscStLyt
import Gpu.Vulkan.Pipeline.Graphics.Middle qualified as M

import Gpu.Vulkan.VertexInput qualified as VertexInput

newtype G s (vibs :: [(Type, VertexInput.Rate)]) (vias :: [(Nat, Type)])
	(lyta :: (Type, [(Type, [DscStLyt.BindingType])], [Type])) = G M.G
