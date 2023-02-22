{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Middle.Internal (
	C(..)
	) where

import Data.IORef

import qualified Gpu.Vulkan.CommandBuffer.Core as C
import qualified Gpu.Vulkan.Pipeline.Core as Pipeline.C

data C = C {
	cPipeline :: IORef Pipeline.C.P,
	unC :: C.C }
