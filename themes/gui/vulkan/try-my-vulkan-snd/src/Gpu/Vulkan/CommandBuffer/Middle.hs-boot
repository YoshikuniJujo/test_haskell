{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Middle where

import Data.Kind
import Data.IORef

import qualified Gpu.Vulkan.CommandBuffer.Core as C
import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C
import qualified Gpu.Vulkan.Pipeline.Layout.Core as Pipeline.Layout.C

data C (vs :: [Type]) = C {
	cPipeline :: IORef Pipeline.C.P,
	unC :: C.C }

