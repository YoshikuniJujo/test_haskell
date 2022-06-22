{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Type where

import Data.Kind.Object

import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as M

newtype L s = L { unL :: M.L } deriving Show

newtype L' s (bts :: [BindingType]) = L' { unL' :: M.L } deriving Show

data BindingType = Buffer [Object] | Other
