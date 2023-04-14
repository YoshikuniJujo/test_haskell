{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueryPool where

import Foreign.Storable
import Foreign.Storable.PeekPoke
import Control.Exception

import Data.Kind
import Data.Word

import Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.Query.Enum qualified as Q
import Gpu.Vulkan.QueryPool.Middle qualified as M

newtype Q sq (tp :: Bool -> Type) = Q M.Q deriving Show

create :: (WithPoked n, QueryType tp, WithPoked c, WithPoked d) =>
	Device.D sd -> CreateInfo n tp ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall sq . Q sq tp -> IO a) -> IO a
create (Device.D dv) ci macc macd f = bracket
	(M.create dv (createInfoToMiddle ci) macc)
	(\qp -> M.destroy dv qp macd) (f . Q)

getResults :: (
	QueryType tp,
	Storable (M.W32W64 w64), M.W32W64Tools w64,
	M.AvailabilityTools av (M.W32W64 w64) ) =>
	Device.D sd -> Q sq tp -> Word32 -> Word32 -> Q.ResultFlags ->
	IO [M.Availability av (tp w64)]
getResults (Device.D dv) (Q qp) fq qc flgs =
	((fromWord <$>) <$>) <$> M.getResults dv qp fq qc flgs

data CreateInfo n (tp :: Bool -> Type) = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlagBits,
	createInfoQueryCount :: Word32,
	createInfoPipelineStatistics :: Q.PipelineStatisticFlags }
	deriving Show

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
	queryType :: Q.Type
	fromWord :: M.W32W64 w64 -> qt w64

instance QueryType PipelineStatistics where
	queryType = Q.TypePipelineStatistics
	fromWord = pipelineStatisticsFromWord

data PipelineStatistics (w64 :: Bool) where
	PipelineStatistics32 :: Word32 -> PipelineStatistics 'False
	PipelineStatistics64 :: Word64 -> PipelineStatistics 'True

deriving instance Show (PipelineStatistics w64)

pipelineStatisticsFromWord :: M.W32W64 w64 -> PipelineStatistics w64
pipelineStatisticsFromWord = \case
	M.W32 w -> PipelineStatistics32 w; M.W64 w -> PipelineStatistics64 w
