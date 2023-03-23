{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Type (G(..), GNew(..), gFromNew, gToNew) where

import GHC.TypeNats
import Data.Kind

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as DscStLyt
import Gpu.Vulkan.Pipeline.Graphics.Middle qualified as M

import Gpu.Vulkan.VertexInput qualified as VertexInput

newtype G s (vs :: [(Type, VertexInput.Rate)]) (ts :: [(Nat, Type)]) = G M.G

newtype GNew s (vs :: [(Type, VertexInput.Rate)]) (ts :: [(Nat, Type)])
	(slbtss :: (Type, [(Type, [DscStLyt.BindingType])], [Type])) = GNew M.G

gFromNew :: GNew s vs ts slbtss -> G s vs ts
gFromNew (GNew mg) = G mg

gToNew :: G s vs ts -> GNew s vs ts slbtss
gToNew (G mg) = GNew mg
