{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Type where

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Image.Middle as M

newtype I s = I M.I

newtype Binded si sm = Binded M.I

newtype INew s (fmt :: T.Format) = INew M.I

newtype BindedNew si sm (fmt :: T.Format) = BindedNew M.I

iFromNew :: INew s fmt -> I s
iFromNew (INew i) = I i

iToNew :: I s -> INew s fmt
iToNew (I i) = INew i

bindedFromNew :: BindedNew si sm fmt -> Binded si sm
bindedFromNew (BindedNew i) = Binded i

bindedToNew :: Binded si sm -> BindedNew si sm fmt
bindedToNew (Binded i) = BindedNew i
