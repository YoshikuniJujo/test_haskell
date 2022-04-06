{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command where

import Foreign.Pointable
import Control.Monad.Cont

import Vulkan

import qualified Vulkan.CommandBuffer as CommandBuffer
import qualified Vulkan.RenderPass as RenderPass
import qualified Vulkan.Subpass.Enum as Subpass
import qualified Vulkan.Command.Core as C

beginRenderPass :: (Pointable n, ClearValueToCore ct) =>
	CommandBuffer.C -> RenderPass.BeginInfo n ct -> Subpass.Contents -> IO ()
beginRenderPass (CommandBuffer.C cb)
	rpbi (Subpass.Contents spcnt) = ($ pure) $ runContT do
	prpbi <- RenderPass.beginInfoToCore rpbi
	lift $ C.beginRenderPass cb prpbi spcnt
