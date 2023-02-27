{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Graphics.Type (G(..)) where

import GHC.TypeNats
import Data.Kind

import qualified Gpu.Vulkan.Pipeline.Graphics.Middle as M

newtype G s (vs :: [Type]) (ts :: [(Nat, Type)]) = G M.G
