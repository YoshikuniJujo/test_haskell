{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineLayout.Type where

import Data.Kind

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout
import qualified Gpu.Vulkan.PipelineLayout.Middle as M

newtype P s
	(sbtss :: [(Type, [DescriptorSetLayout.BindingType])]) (pcw :: [Type]) =
	P M.P deriving Show
