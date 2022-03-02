{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr where

import qualified Vulkan.Khr.Surface.Core as Surface.C

newtype Surface = Surface Surface.C.Surface deriving Show
