{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.List.Type where

import qualified Vulkan.Buffer.List.Middle as M

newtype L s v = L (M.B v) deriving Show

newtype Binded sl sm v = Binded (M.B v) deriving Show
