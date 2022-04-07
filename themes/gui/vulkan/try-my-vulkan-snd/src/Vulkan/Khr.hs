{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr where

import qualified Data.Text as T

validationLayerName :: T.Text
validationLayerName = "VK_LAYER_KHRONOS_validation"

-- aquireNextImage :: Device.D ->
