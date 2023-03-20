{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Type where

import Data.Kind

import Gpu.Vulkan.CommandBuffer.Middle qualified as M

newtype C s = C { unC :: M.C }

newtype Binded s (vs :: [Type]) = Binded { unBinded :: M.C }

toBinded :: C s -> Binded s vs
toBinded = Binded . unC

fromBinded :: Binded s vs -> C s
fromBinded = C . unBinded
