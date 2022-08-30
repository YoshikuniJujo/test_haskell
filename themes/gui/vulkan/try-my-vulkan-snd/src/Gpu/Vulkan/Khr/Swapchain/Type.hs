{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Type where

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Khr.Swapchain.Middle as M

newtype S ss = S M.S deriving Show

newtype SNew ss (fmt :: T.Format) = SNew M.S deriving Show

sFromNew :: SNew ss (fmt :: T.Format) -> S ss
sFromNew (SNew s) = S s

sToNew :: S ss -> SNew ss (fmt :: T.Format)
sToNew (S s) = SNew s
