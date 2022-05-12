{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler where

import qualified Vulkan.Sampler.Core as C

newtype S = S C.S deriving Show
