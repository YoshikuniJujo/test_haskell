{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Internal (
	M(..), M.CreateInfo(..), M.CreateFlags, create, destroy ) where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Uncurry

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.ShaderModule.Middle as M

data M mn sknd mscc = M {
	mCreateInfo :: M.CreateInfo mn sknd,
	mAllocationCallbacks :: TPMaybe.M (U2 AllocationCallbacks.A) mscc }

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mscc) =>
	Device.D sd -> M mn sknd mscc -> IO (M.M sknd)
create (Device.D dvc) m =
	M.create dvc (mCreateInfo m)
		. AllocationCallbacks.toMiddle $ mAllocationCallbacks m

destroy :: AllocationCallbacks.ToMiddle mscc =>
	Device.D sd -> M.M sknd -> M n sknd mscc -> IO ()
destroy (Device.D dvc) mm m =
	M.destroy dvc mm . AllocationCallbacks.toMiddle $ mAllocationCallbacks m
