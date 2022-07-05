module Gpu.Vulkan.Ext.DebugUtils.Messenger.Type where

import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger.Middle as M

newtype M sm = M M.M deriving Show
