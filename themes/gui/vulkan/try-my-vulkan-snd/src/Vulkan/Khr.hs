{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr where

import qualified Vulkan.Khr.Surface as Surface

newtype Surface = Surface Surface.Surface deriving Show
