{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr where

import qualified Data.Text as T

import qualified Vulkan.Khr.Surface.Core as Surface.C
import qualified Vulkan.Khr.Swapchain.Core as Swapchain.C

newtype Surface = Surface Surface.C.Surface deriving Show

newtype Swapchain = Swapchain Swapchain.C.Swapchain deriving Show

validationLayerName :: T.Text
validationLayerName = "VK_LAYER_KHRONOS_validation"
