{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
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

data W32W64 (w64 :: Bool) where
	W32 :: Word32 -> W32W64 'False
	W64 :: Word64 -> W32W64 'True

deriving instance Show (W32W64 w64)

instance Storable (W32W64 'False) where
	sizeOf _ = sizeOf @Word32 undefined
	alignment _ = alignment @Word32 undefined
	peek p = W32 <$> peek (castPtr p)
	poke p (W32 w) = poke (castPtr p) w

instance Storable (W32W64 'True) where
	sizeOf _ = sizeOf @Word64 undefined
	alignment _ = alignment @Word64 undefined
	peek p = W64 <$> peek (castPtr p)
	poke p (W64 w) = poke (castPtr p) w

class W32W64Tools (w64 :: Bool) where
	bytesOf :: Integral n => n
	result64Bit :: ResultFlags -> ResultFlags

instance W32W64Tools False where
	bytesOf = 4
	result64Bit = (.&. complement Result64Bit)

instance W32W64Tools True where
	bytesOf = 8
	result64Bit = (.|. Result64Bit)

getResultsW32W64 :: forall w64 . (Storable (W32W64 w64), W32W64Tools w64) =>
	Device.D -> Q -> Word32 -> Word32 -> ResultFlags -> IO [W32W64 w64]
getResultsW32W64
	(Device.D dv) (Q q) fq qc (result64Bit @w64 -> ResultFlagBits flgs) =
	allocaArray (qc' * 2) \pd -> do
		r <- C.getResults dv q fq qc
			(qc' * 2 * bytesOf @w64) pd (2 * bytesOf @w64) flgs
		throwUnlessSuccess $ Result r
		peekArray (qc' * 2) $ castPtr pd
	where
	qc' :: Integral n => n
	qc' = fromIntegral qc
