{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.DebugUtils where

import Foreign.C.String
import Data.String
import System.IO.Unsafe

foreign import capi "vulkan/vulkan.h value VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
	c_extensionName :: CString

extensionName :: IsString s => s
extensionName = unsafePerformIO $ fromString <$> peekCString c_extensionName
