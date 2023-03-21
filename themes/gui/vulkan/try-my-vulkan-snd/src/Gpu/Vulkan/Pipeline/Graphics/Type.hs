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

newtype G s (vs :: [Type]) (ts :: [(Nat, Type)]) = G M.G

newtype GNew s (vs :: [Type]) (ts :: [(Nat, Type)])
	(slbtss :: (Type, [(Type, [DscStLyt.BindingType])], [Type])) = GNew M.G
