{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Semaphore where

import qualified Vulkan.Semaphore.Core as C

newtype S = S { unS :: C.S }

instance Show S
