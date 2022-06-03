{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Cache.Type where

import qualified Vulkan.Pipeline.Cache.Middle as M

newtype C s = C M.C deriving Show

cToMiddle :: C s -> M.C
cToMiddle (C c) = c
