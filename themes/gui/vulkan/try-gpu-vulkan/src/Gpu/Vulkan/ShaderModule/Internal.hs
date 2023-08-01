{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ShaderModule.Internal (

	-- * CREATE AND DESTROY

	create, destroy, M.CreateInfo(..), M.CreateFlags

	) where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.ShaderModule.Middle as M

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mscc) =>
	Device.D sd -> M.CreateInfo mn sknd -> TPMaybe.M (U2 AllocationCallbacks.A) mscc -> IO (M.S sknd)
create (Device.D dvc) m mac =
	M.create dvc m $ AllocationCallbacks.toMiddle mac

destroy :: AllocationCallbacks.ToMiddle mscc =>
	Device.D sd -> M.S sknd -> TPMaybe.M (U2 AllocationCallbacks.A) mscc -> IO ()
destroy (Device.D dvc) mm mac =
	M.destroy dvc mm $ AllocationCallbacks.toMiddle mac
