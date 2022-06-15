{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.List.Type where

import Prelude hiding (length)

import qualified Vulkan.Buffer.List.Middle as M

newtype L s v = L (M.B v) deriving Show

length :: L s v -> Int
length (L (M.B ln _)) = ln

newtype Binded sl sm v = Binded (M.B v) deriving Show
