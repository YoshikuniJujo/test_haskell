{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.List (allocate, M.AllocateInfo(..), writeMono, readList) where

import Prelude hiding (readList)

import Foreign.Pointable
import Control.Exception
import Data.MonoTraversable

import qualified Foreign.Storable.Generic

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Memory.List.Middle as M
import qualified Gpu.Vulkan.Buffer.List.Type as Buffer.List

allocate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D sd -> Buffer.List.L sb v -> M.AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . Device.MemoryList s v -> IO a) -> IO a
allocate (Device.D dvc) (Buffer.List.L bf) ai macc macd f = bracket
	(M.allocate dvc bf ai macc) (\mem -> M.free dvc mem macd)
	(f . Device.MemoryList)

writeMono :: (
	Foreign.Storable.Generic.G (Element vs), MonoFoldable vs ) =>
	Device.D sd ->
	Device.MemoryList sm (Element vs) -> Memory.M.MapFlags -> vs -> IO ()
writeMono (Device.D dvc) (Device.MemoryList mem) flgs vs =
	M.writeMono dvc mem flgs vs

readList :: Foreign.Storable.Generic.G v =>
	Device.D sd -> Device.MemoryList sm v -> Memory.M.MapFlags -> IO [v]
readList (Device.D dvc) (Device.MemoryList mem) flgs = M.readList dvc mem flgs
