{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueryPool where

import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Kind
import Data.Word

import Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import Gpu.Vulkan.PhysicalDevice qualified as PhysicalDevice
import Gpu.Vulkan.PhysicalDevice.Struct qualified as PhysicalDevice
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.Query.Enum qualified as Q
import Gpu.Vulkan.QueryPool.Middle qualified as M

newtype Q sq (tp :: Bool -> Type) = Q M.Q deriving Show

create :: (
	WithPoked (TMaybe.M mn), QueryType tp,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo mn tp ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall sq . Q sq tp -> IO a) -> IO a
create (Device.D dv) ci
	(AllocationCallbacks.toMiddle -> macc) f = bracket
	(M.create dv (createInfoToMiddle ci) macc)
	(\qp -> M.destroy dv qp macc) (f . Q)

getResults :: forall sd sq av tp w64 . (
	QueryType tp,
	Storable (M.W32W64 w64), M.W32W64Tools w64,
	M.AvailabilityTools av (M.W32W64 w64) ) =>
	PhysicalDevice.P ->
	Device.D sd -> Q sq tp -> Word32 -> Word32 -> Q.ResultFlags ->
	IO [M.Availability av (tp w64)]
getResults pd (Device.D dv) (Q qp) fq qc flgs = do
	a <- getQueryArg @tp pd
	((fromWord a <$>) <$>) <$> M.getResults dv qp fq qc flgs

data CreateInfo mn (tp :: Bool -> Type) = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: M.CreateFlags,
	createInfoQueryCount :: Word32,
	createInfoPipelineStatistics :: Q.PipelineStatisticFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn tp)

createInfoToMiddle ::
	forall n tp . QueryType tp => CreateInfo n tp -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoQueryCount = qc,
	createInfoPipelineStatistics = ps } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoQueryType = queryType @tp,
	M.createInfoQueryCount = qc,
	M.createInfoPipelineStatistics = ps }

class QueryType (qt :: Bool -> Type) where
	type QueryArg qt
	queryType :: Q.Type
	fromWord :: QueryArg qt -> M.W32W64 w64 -> qt w64
	getQueryArg :: PhysicalDevice.P -> IO (QueryArg qt)

instance QueryType PipelineStatistics where
	type QueryArg PipelineStatistics = ()
	queryType = Q.TypePipelineStatistics
	fromWord () = pipelineStatisticsFromWord
	getQueryArg _ = pure ()

data PipelineStatistics (w64 :: Bool) where
	PipelineStatistics32 :: Word32 -> PipelineStatistics 'False
	PipelineStatistics64 :: Word64 -> PipelineStatistics 'True

deriving instance Show (PipelineStatistics w64)

pipelineStatisticsFromWord :: M.W32W64 w64 -> PipelineStatistics w64
pipelineStatisticsFromWord = \case
	M.W32 w -> PipelineStatistics32 w; M.W64 w -> PipelineStatistics64 w

data Timestamp w64 where
	Timestamp32 :: {
		timestampPeriod32 :: Float,
		timestampW32 :: Word32 } -> Timestamp 'False
	Timestamp64 :: {
		timestampPeriod64 :: Float,
		timestampW64 :: Word64 } -> Timestamp 'True

-- deriving instance Show (Timestamp w64)

instance Show (Timestamp w64) where
	show = \case
		Timestamp32 p w -> show @Double (realToFrac p * fromIntegral w) ++ "ns"
		Timestamp64 p w -> show @Double (realToFrac p * fromIntegral w) ++ "ns"

instance QueryType Timestamp where
	type QueryArg Timestamp = Float
	queryType = Q.TypeTimestamp
	fromWord p = \case
		M.W32 w -> Timestamp32 p w; M.W64 w -> Timestamp64 p w
	getQueryArg pd = do
		lmts <- PhysicalDevice.propertiesLimits <$> PhysicalDevice.getProperties pd
		pure $ PhysicalDevice.limitsTimestampPeriod lmts

type FirstQuery = Word32
type QueryCount = Word32
