{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImageView.Type where

import GHC.TypeLits

import Gpu.Vulkan.TypeEnum as T
import Gpu.Vulkan.ImageView.Middle qualified as M

newtype I (nm :: Symbol) (fmt :: T.Format) si = I M.I deriving Show
