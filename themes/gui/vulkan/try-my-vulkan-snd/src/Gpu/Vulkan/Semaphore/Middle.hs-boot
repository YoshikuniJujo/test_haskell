{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Middle where

import qualified Gpu.Vulkan.Semaphore.Core as C

newtype S = S { unS :: C.S }

instance Show S
