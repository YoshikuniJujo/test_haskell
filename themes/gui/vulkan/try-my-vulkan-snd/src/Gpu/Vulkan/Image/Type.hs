{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Type where

import GHC.TypeLits

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Image.Middle as M

newtype I s = I M.I

newtype INew s (nm :: Symbol) (fmt :: T.Format) = INew M.I

newtype BindedNew sm si (nm :: Symbol) (fmt :: T.Format) = BindedNew M.I

iFromNew :: INew s nm fmt -> I s
iFromNew (INew i) = I i

iToNew :: I s -> INew s nm fmt
iToNew (I i) = INew i
