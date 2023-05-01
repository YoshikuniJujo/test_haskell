{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Internal (
	M(..), M.CreateInfo(..), M.CreateFlags, create, destroy ) where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.ShaderModule.Middle as M

data M mn sknd sc c sd' d = M {
	mCreateInfo :: M.CreateInfo mn sknd,
	mAllocationCallbacksCreate :: Maybe (AllocationCallbacks.A sc c),
	mAllocationCallbacksDestroy :: Maybe (AllocationCallbacks.A sd' d) }

create :: (WithPoked (TMaybe.M mn), Pokable c) =>
	Device.D sd -> M mn sknd sc c sd' d -> IO (M.M sknd)
create (Device.D dvc) m =
	M.create dvc (mCreateInfo m)
		. (AllocationCallbacks.toMiddle <$>) $ mAllocationCallbacksCreate m

destroy :: Pokable d => Device.D sd -> M.M sknd -> M n sknd sc c sd' d -> IO ()
destroy (Device.D dvc) mm m =
	M.destroy dvc mm . (AllocationCallbacks.toMiddle <$>) $ mAllocationCallbacksDestroy m
