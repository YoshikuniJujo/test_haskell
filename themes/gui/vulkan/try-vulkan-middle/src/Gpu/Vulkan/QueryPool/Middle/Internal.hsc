{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
 
module Gpu.Vulkan.QueryPool.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.Bits
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import Gpu.Vulkan.Device.Middle.Internal qualified as Device
import Gpu.Vulkan.Query.Enum
import Gpu.Vulkan.QueryPool.Core qualified as C

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkQueryPoolCreateFlags}
		[''Show, ''Eq, ''Storable, ''Bits] []

type CreateFlags = CreateFlagBits

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlagBits,
	createInfoQueryType :: Type,
	createInfoQueryCount :: Word32,
	createInfoPipelineStatistics :: PipelineStatisticFlags }
	deriving Show

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueryType = Type tp,
	createInfoQueryCount = cnt,
	createInfoPipelineStatistics = PipelineStatisticFlagBits ps } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	withPoked C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoQueryType = tp,
		C.createInfoQueryCount = cnt,
		C.createInfoPipelineStatistics = ps } f

newtype Q = Q C.Q deriving Show

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO Q
create (Device.D dv) ci mac = Q <$> alloca \pq -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCore mac \pac -> do
			r <- C.create dv pci pac pq
			throwUnlessSuccess $ Result r
	peek pq

destroy :: WithPoked d =>
	Device.D -> Q -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dv) (Q q) mad = AllocationCallbacks.maybeToCore mad \pad ->
	C.destroy dv q pad

reset :: Device.D -> Q -> Word32 -> Word32 -> IO ()
reset (Device.D dv) (Q q) fq qc = C.reset dv q fq qc

getResultsRaw :: Device.D -> Q -> Word32 -> Word32 -> ResultFlags -> IO [Word32]
getResultsRaw (Device.D dv) (Q q) fq qc (ResultFlagBits flgs) =
	allocaArray (qc' * 16) \pd -> do
		r <- C.getResults dv q fq qc (qc' * 16) pd 16 flgs
		throwUnlessSuccess $ Result r
		peekArray (qc' * 4) $ castPtr pd
	where
	qc' :: Integral n => n
	qc' = fromIntegral qc
