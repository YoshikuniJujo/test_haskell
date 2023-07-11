{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Type where

import GHC.TypeLits

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Image.Middle as M

newtype I s (nm :: Symbol) (fmt :: T.Format) = I M.I

newtype Binded sm si (nm :: Symbol) (fmt :: T.Format) = Binded M.I
