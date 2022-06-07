{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Shader.Module.Internal (
	M(..), M.CreateInfo(..), M.CreateFlags, pattern M.CreateFlagsZero,
	create, destroy ) where

import Foreign.Pointable

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Shader.Module.Middle as M

data M n sknd a a' = M {
	mCreateInfo :: M.CreateInfo n sknd,
	mAllocationCallbacksCreate :: Maybe (AllocationCallbacks.A a),
	mAllocationCallbacksDestroy :: Maybe (AllocationCallbacks.A a') }

create :: (Pointable n, Pointable a) =>
	Device.D sd -> M n sknd a a' -> IO (M.M sknd)
create (Device.D dvc) m =
	M.create dvc (mCreateInfo m) (mAllocationCallbacksCreate m)

destroy :: (Pointable a') => Device.D sd -> M.M sknd -> M n sknd a a' -> IO ()
destroy (Device.D dvc) mm m = M.destroy dvc mm (mAllocationCallbacksDestroy m)
