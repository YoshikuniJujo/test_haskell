{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Internal (
	M(..), M.CreateInfo(..), M.CreateFlags, pattern M.CreateFlagsZero,
	create, destroy ) where

import Foreign.Pointable

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.ShaderModule.Middle as M

data M n sknd c d = M {
	mCreateInfo :: M.CreateInfo n sknd,
	mAllocationCallbacksCreate :: Maybe (AllocationCallbacks.A c),
	mAllocationCallbacksDestroy :: Maybe (AllocationCallbacks.A d) }

create :: (Pointable n, Pointable c) =>
	Device.D sd -> M n sknd c d -> IO (M.M sknd)
create (Device.D dvc) m =
	M.create dvc (mCreateInfo m) (mAllocationCallbacksCreate m)

destroy :: (Pointable d) => Device.D sd -> M.M sknd -> M n sknd c d -> IO ()
destroy (Device.D dvc) mm m = M.destroy dvc mm (mAllocationCallbacksDestroy m)
