{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer (
	C, allocateOld, allocateNew, AllocateInfoOld(..), AllocateInfoNew(..), begin, M.BeginInfo(..), reset,
	
	Level,
	pattern LevelPrimary, pattern LevelSecondary, pattern LevelMaxEnum,

	UsageFlags, UsageFlagBits,
	pattern UsageOneTimeSubmitBit, pattern UsageRenderPassContinueBit,
	pattern UsageSimultaneousUseBit, pattern UsageFlagBitsMaxEnum,
	
	ResetFlags, ResetFlagBits,
	pattern ResetReleaseResourcesBit, pattern ResetFlagBitsMaxEnum

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import qualified Data.HeteroParList as HeteroParList
import Data.Word

import qualified TypeLevel.List as TpLvlLst

import Gpu.Vulkan.CommandBuffer.Type
import Gpu.Vulkan.CommandBuffer.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.CommandPool.Type as CommandPool
import qualified Gpu.Vulkan.CommandPool.Middle as CommandPool.M
import qualified Gpu.Vulkan.CommandBuffer.Middle as M

data AllocateInfoOld n s = AllocateInfoOld {
	allocateInfoNextOld :: Maybe n,
	allocateInfoCommandPoolOld :: CommandPool.C s,
	allocateInfoLevelOld :: Level,
	allocateInfoCommandBufferCountOld :: Word32 }
	deriving Show

allocateInfoToMiddleOld :: AllocateInfoOld n s -> M.AllocateInfo n
allocateInfoToMiddleOld AllocateInfoOld {
	allocateInfoNextOld = nxt,
	allocateInfoCommandPoolOld = CommandPool.C cp,
	allocateInfoLevelOld = lvl,
	allocateInfoCommandBufferCountOld = cnt } = M.AllocateInfo {
	M.allocateInfoNext = nxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount = cnt }

data AllocateInfoNew n s (vss :: [[Type]]) = AllocateInfoNew {
	allocateInfoNextNew :: Maybe n,
	allocateInfoCommandPoolNew :: CommandPool.C s,
	allocateInfoLevelNew :: Level }
	deriving Show

allocateInfoToMiddleNew :: AllocateInfoNew n s vss -> AllocateInfoNewM n vss
allocateInfoToMiddleNew AllocateInfoNew {
	allocateInfoNextNew = nxt,
	allocateInfoCommandPoolNew = CommandPool.C cp,
	allocateInfoLevelNew = lvl } = AllocateInfoNewM {
	allocateInfoNextNewM = nxt,
	allocateInfoCommandPoolNewM = cp,
	allocateInfoLevelNewM = lvl }

allocateNew ::
	(WithPoked n, TpLvlLst.Length [Type] vss, HeteroParList.FromList vss) =>
	Device.D sd -> AllocateInfoNew n scp vss ->
	(forall s . HeteroParList.PL (C s) vss -> IO a) -> IO a
allocateNew (Device.D dvc) (allocateInfoToMiddleNew -> ai) f = bracket
	(allocateNewM dvc ai) (freeCsNew dvc $ allocateInfoCommandPoolNewM ai)
	(f . HeteroParList.map C)

allocateOld :: WithPoked n =>
	Device.D sd -> AllocateInfoOld n sp ->
	(forall s . [C s vs] -> IO a) -> IO a
allocateOld (Device.D dvc) (allocateInfoToMiddleOld -> ai) f = bracket
	(M.allocate dvc ai) (M.freeCs dvc (M.allocateInfoCommandPool ai))
	(f . (C . CC <$>))

begin :: (WithPoked n, WithPoked n') =>
	C s vs -> M.BeginInfo n n' -> IO a -> IO a
begin (C (CC cb)) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: C sc vs -> ResetFlags -> IO ()
reset (C (CC cb)) rfs = M.reset cb rfs

allocateNewM ::
	(WithPoked n, TpLvlLst.Length [Type] vss, HeteroParList.FromList vss) =>
	Device.M.D -> AllocateInfoNewM n vss -> IO (HeteroParList.PL CC vss)
allocateNewM dvc ai = HeteroParList.fromList CC <$> M.allocate dvc (allocateInfoFromNew ai)

freeCsNew :: Device.M.D -> CommandPool.M.C -> HeteroParList.PL CC vss -> IO ()
freeCsNew dvc cp cs =
	M.freeCs dvc cp (HeteroParList.toList (\(CC cb) -> cb) cs)

data AllocateInfoNewM n (vss :: [[Type]]) = AllocateInfoNewM {
	allocateInfoNextNewM :: Maybe n,
	allocateInfoCommandPoolNewM :: CommandPool.M.C,
	allocateInfoLevelNewM :: Level }
	deriving Show

allocateInfoFromNew :: forall n vss .
	TpLvlLst.Length [Type] vss =>
	AllocateInfoNewM n vss -> M.AllocateInfo n
allocateInfoFromNew AllocateInfoNewM {
	allocateInfoNextNewM = mnxt,
	allocateInfoCommandPoolNewM = cp,
	allocateInfoLevelNewM = lvl } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount =
		fromIntegral (TpLvlLst.length @_ @vss) }
