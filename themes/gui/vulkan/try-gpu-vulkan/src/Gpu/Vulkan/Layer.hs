{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Layer (khronosValidationName) where

import Data.Text qualified as T

khronosValidationName :: T.Text
khronosValidationName = "VK_LAYER_KHRONOS_validation"
