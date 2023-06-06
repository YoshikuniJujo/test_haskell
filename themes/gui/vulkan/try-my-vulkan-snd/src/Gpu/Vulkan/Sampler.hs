{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sampler where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Uncurry

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Sampler.Middle as M

newtype S ss = S M.S deriving Show

sToMiddle :: S ss -> M.S
sToMiddle (S s) = s

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mscc) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall ss . S ss -> IO a) -> IO a
create (Device.D dvc) ci
	(AllocationCallbacks.toMiddle -> macc) f =
	bracket (M.create dvc ci macc) (\s -> M.destroy dvc s macc) (f . S)
