{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Type where

import Data.Kind

import Gpu.Vulkan.CommandBuffer.Middle as M

newtype Binded s (vs :: [Type]) = Binded { unBinded :: M.C }
