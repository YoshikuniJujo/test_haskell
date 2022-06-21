{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Cache.Type where

import qualified Gpu.Vulkan.Pipeline.Cache.Middle as M

newtype C s = C M.C deriving Show

cToMiddle :: C s -> M.C
cToMiddle (C c) = c
