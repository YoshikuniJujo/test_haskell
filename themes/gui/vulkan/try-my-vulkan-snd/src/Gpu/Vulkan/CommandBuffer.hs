{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer (
	C, allocateOld, allocate, AllocateInfoOld(..), AllocateInfo(..), begin, M.BeginInfo(..), reset,
	
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

data AllocateInfo n s (vss :: [[Type]]) = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoCommandPool :: CommandPool.C s,
	allocateInfoLevel :: Level }
	deriving Show

allocate ::
	(WithPoked n, TpLvlLst.Length [Type] vss, HeteroParList.FromList vss) =>
	Device.D sd -> AllocateInfo n scp vss ->
	(forall s . HeteroParList.PL (C s) vss -> IO a) -> IO a
allocate (Device.D dvc) ai_ f = bracket
	(allocateM dvc ai) (freeCs dvc $ allocateInfoCommandPoolM ai)
	(f . HeteroParList.fromList C)
	where
	ai = allocateInfoToMiddle ai_
	allocateM ::
		(WithPoked n, TpLvlLst.Length [Type] vss, HeteroParList.FromList vss) =>
		Device.M.D -> AllocateInfoM n vss -> IO [M.C]
	allocateM dvc ai = M.allocate dvc (allocateInfoFrom ai)

	freeCs :: Device.M.D -> CommandPool.M.C -> [M.C] -> IO ()
	freeCs dvc cp cs = M.freeCs dvc cp cs

	allocateInfoFrom :: forall n vss .
		TpLvlLst.Length [Type] vss =>
		AllocateInfoM n vss -> M.AllocateInfo n
	allocateInfoFrom AllocateInfoM {
		allocateInfoNextM = mnxt,
		allocateInfoCommandPoolM = cp,
		allocateInfoLevelM = lvl } = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoCommandPool = cp,
		M.allocateInfoLevel = lvl,
		M.allocateInfoCommandBufferCount =
			fromIntegral (TpLvlLst.length @_ @vss) }

	allocateInfoToMiddle :: AllocateInfo n s vss -> AllocateInfoM n vss
	allocateInfoToMiddle AllocateInfo {
		allocateInfoNext = nxt,
		allocateInfoCommandPool = CommandPool.C cp,
		allocateInfoLevel = lvl } = AllocateInfoM {
		allocateInfoNextM = nxt,
		allocateInfoCommandPoolM = cp,
		allocateInfoLevelM = lvl }

data AllocateInfoM n (vss :: [[Type]]) = AllocateInfoM {
	allocateInfoNextM :: Maybe n,
	allocateInfoCommandPoolM :: CommandPool.M.C,
	allocateInfoLevelM :: Level }
	deriving Show

allocateOld :: WithPoked n =>
	Device.D sd -> AllocateInfoOld n sp ->
	(forall s . [C s vs] -> IO a) -> IO a
allocateOld (Device.D dvc) (allocateInfoToMiddleOld -> ai) f = bracket
	(M.allocate dvc ai) (M.freeCs dvc (M.allocateInfoCommandPool ai))
	(f . (C <$>))

begin :: (WithPoked n, WithPoked n') =>
	C s vs -> M.BeginInfo n n' -> IO a -> IO a
begin (C cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: C sc vs -> ResetFlags -> IO ()
reset (C cb) rfs = M.reset cb rfs
