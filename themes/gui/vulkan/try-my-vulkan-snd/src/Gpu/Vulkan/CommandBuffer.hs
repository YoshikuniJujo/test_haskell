{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer (
	C, allocate, allocateNew, AllocateInfo(..), AllocateInfoNew(..), begin, M.BeginInfo(..), reset,
	
	Level,
	pattern LevelPrimary, pattern LevelSecondary, pattern LevelMaxEnum,

	UsageFlags, UsageFlagBits,
	pattern UsageOneTimeSubmitBit, pattern UsageRenderPassContinueBit,
	pattern UsageSimultaneousUseBit, pattern UsageFlagBitsMaxEnum,
	
	ResetFlags, ResetFlagBits,
	pattern ResetReleaseResourcesBit, pattern ResetFlagBitsMaxEnum

	) where

import Foreign.Storable
import Control.Exception
import Data.Kind
import Data.HeteroList
import Data.Word

import qualified TypeLevel.List as TpLvlLst

import Gpu.Vulkan.CommandBuffer.Type
import Gpu.Vulkan.CommandBuffer.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.CommandPool.Type as CommandPool
import qualified Gpu.Vulkan.CommandPool.Middle as CommandPool.M
import qualified Gpu.Vulkan.CommandBuffer.Middle as M

data AllocateInfo n s = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoCommandPool :: CommandPool.C s,
	allocateInfoLevel :: Level,
	allocateInfoCommandBufferCount :: Word32 }
	deriving Show

allocateInfoToMiddle :: AllocateInfo n s -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = nxt,
	allocateInfoCommandPool = CommandPool.C cp,
	allocateInfoLevel = lvl,
	allocateInfoCommandBufferCount = cnt } = M.AllocateInfo {
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
	(Storable n, TpLvlLst.Length [Type] vss, ListToHeteroVarList vss) =>
	Device.D sd -> AllocateInfoNew n scp vss ->
	(forall s . HeteroVarList (C s) vss -> IO a) -> IO a
allocateNew (Device.D dvc) (allocateInfoToMiddleNew -> ai) f = bracket
	(allocateNewM dvc ai) (freeCsNew dvc $ allocateInfoCommandPoolNewM ai)
	(f . heteroVarListMap C)

allocate :: Storable n =>
	Device.D sd -> AllocateInfo n sp ->
	(forall s . [C s vs] -> IO a) -> IO a
allocate (Device.D dvc) (allocateInfoToMiddle -> ai) f = bracket
	(M.allocate dvc ai) (M.freeCs dvc (M.allocateInfoCommandPool ai))
	(f . (C . CC <$>))

begin :: (Storable n, Storable n') =>
	C s vs -> M.BeginInfo n n' -> IO a -> IO a
begin (C (CC cb)) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: C sc vs -> ResetFlags -> IO ()
reset (C (CC cb)) rfs = M.reset cb rfs

allocateNewM ::
	(Storable n, TpLvlLst.Length [Type] vss, ListToHeteroVarList vss) =>
	Device.M.D -> AllocateInfoNewM n vss -> IO (HeteroVarList CC vss)
allocateNewM dvc ai = listToHeteroVarList CC <$> M.allocate dvc (allocateInfoFromNew ai)

freeCsNew :: Device.M.D -> CommandPool.M.C -> HeteroVarList CC vss -> IO ()
freeCsNew dvc cp cs =
	M.freeCs dvc cp (heteroVarListToList (\(CC cb) -> cb) cs)

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
