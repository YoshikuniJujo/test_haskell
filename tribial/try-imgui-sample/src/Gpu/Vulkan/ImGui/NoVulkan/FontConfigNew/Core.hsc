{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.NoVulkan.FontConfigNew.Core where

import Foreign.Ptr

data FTag
type F = Ptr FTag

foreign import ccall "im_font_config_new" cxx_im_font_config_new :: IO F
