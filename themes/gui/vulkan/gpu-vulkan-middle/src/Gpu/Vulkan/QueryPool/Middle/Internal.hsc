{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
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
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Bits
import Data.Bool
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

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoQueryType :: Type,
	createInfoQueryCount :: Word32,
	createInfoPipelineStatistics :: PipelineStatisticFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueryType = Type tp,
	createInfoQueryCount = cnt,
	createInfoPipelineStatistics = PipelineStatisticFlagBits ps } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	withPoked C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoQueryType = tp,
		C.createInfoQueryCount = cnt,
		C.createInfoPipelineStatistics = ps } f

newtype Q = Q C.Q deriving Show

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO Q
create (Device.D dv) ci mac = Q <$> alloca \pq -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac -> do
			r <- C.create dv pci pac pq
			throwUnlessSuccess $ Result r
	peek pq

destroy :: Device.D -> Q -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dv) (Q q) mad = AllocationCallbacks.mToCore mad \pad ->
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
deriving instance Eq (W32W64 w64)

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

instance Num (W32W64 'False) where
	W32 m + W32 n = W32 $ m + n
	W32 m * W32 n = W32 $ m * n
	abs (W32 n) = W32 $ abs n
	signum (W32 n) = W32 $ signum n
	fromInteger = W32 . fromInteger
	negate (W32 n) = W32 $ negate n

instance Num (W32W64 'True) where
	W64 m + W64 n = W64 $ m + n
	W64 m * W64 n = W64 $ m * n
	abs (W64 n) = W64 $ abs n
	signum (W64 n) = W64 $ signum n
	fromInteger = W64 . fromInteger
	negate (W64 n) = W64 $ negate n

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

data Availability (av :: Bool) a where
	NonAvailability :: a -> Availability 'False a
	Availability :: Maybe a -> Availability 'True a

deriving instance Show a => Show (Availability av a)

instance Functor (Availability av) where
	fmap f = \case
		NonAvailability x -> NonAvailability $ f x
		Availability mx -> Availability $ f <$> mx

class AvailabilityTools (av :: Bool) a where
	numOfWords :: Integral n => n
	resultWithAvailBit :: ResultFlags -> ResultFlags
	mkAvailability :: [a] -> [Availability av a]

instance AvailabilityTools 'False a where
	numOfWords = 1
	resultWithAvailBit = (.&. complement ResultWithAvailabilityBit)
	mkAvailability = (NonAvailability <$>)

instance (Eq n, Num n) => AvailabilityTools 'True n where
	numOfWords = 2
	resultWithAvailBit = (.|. ResultWithAvailabilityBit)
	mkAvailability = \case
		[] -> []
		[_] -> error "never occur"
		x : a : xas -> Availability
			(bool Nothing (Just x) (a /= 0)) : mkAvailability xas

getResults :: forall av w64 . (
	Storable (W32W64 w64), W32W64Tools w64,
	AvailabilityTools av (W32W64 w64) ) =>
	Device.D -> Q -> Word32 -> Word32 -> ResultFlags ->
	IO [Availability av (W32W64 w64)]
getResults (Device.D dv) (Q q) fq qc
	(resultWithAvailBit @av @(W32W64 w64) . result64Bit @w64 -> ResultFlagBits flgs) =
	allocaArray (qc' * nw) \pd -> do
		r <- C.getResults dv q fq qc
			(qc' * nw * bytesOf @w64) pd (nw * bytesOf @w64) flgs
		throwUnlessSuccess $ Result r
		mkAvailability <$> peekArray (qc' * nw) (castPtr pd)
	where
	qc' :: Integral n => n
	qc' = fromIntegral qc
	nw :: Integral n => n
	nw = numOfWords @av @(W32W64 w64)
