{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Type where

import Data.Kind

import {-# SOURCE #-} qualified Gpu.Vulkan.CommandBuffer.Middle.Internal as M

newtype C s vs = C { unC :: CC vs }

newtype CC (vs :: [Type]) = CC { unCC :: M.MC }
