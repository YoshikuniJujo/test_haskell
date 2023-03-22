{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer (
	C, Binded,

	allocateNew, AllocateInfoNew(..),
	allocate, AllocateInfo(..),
	allocateOld, AllocateInfoOld(..),

	begin, beginNew, M.BeginInfo(..), reset, resetNew,
	
	Level,
	pattern LevelPrimary, pattern LevelSecondary, pattern LevelMaxEnum,

	UsageFlags, UsageFlagBits,
	pattern UsageOneTimeSubmitBit, pattern UsageRenderPassContinueBit,
	pattern UsageSimultaneousUseBit, pattern UsageFlagBitsMaxEnum,
	
	ResetFlags, ResetFlagBits,
	pattern ResetReleaseResourcesBit, pattern ResetFlagBitsMaxEnum

	) where

import GHC.TypeNats
import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.Proxy
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import qualified TypeLevel.List as TpLvlLst

import Gpu.Vulkan.CommandBuffer.Type
import Gpu.Vulkan.CommandBuffer.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.CommandPool.Type as CommandPool
import qualified Gpu.Vulkan.CommandBuffer.Middle as M

allocateNew :: (
	WithPoked n, KnownNat c,
	HeteroParList.FromList (HeteroParList.Dummies c) ) =>
	Device.D sd -> AllocateInfoNew n scp c ->
	(forall s . HeteroParList.LL' (C s) c -> IO a) -> IO a
allocateNew (Device.D dvc) ai f = bracket
	(M.allocate dvc $ allocateInfoToMiddleNew ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPoolNew ai)
	(f . HeteroParList.fromList (HeteroParList.Dummy . C))

data AllocateInfoNew n s (c :: Nat) = AllocateInfoNew {
	allocateInfoNextNew :: Maybe n,
	allocateInfoCommandPoolNew :: CommandPool.C s,
	allocateInfoLevelNew :: Level }
	deriving Show

allocateInfoToMiddleNew :: forall n s c . KnownNat c =>
	AllocateInfoNew n s c -> M.AllocateInfo n
allocateInfoToMiddleNew AllocateInfoNew {
	allocateInfoNextNew = mnxt,
	allocateInfoCommandPoolNew = CommandPool.C cp,
	allocateInfoLevelNew = lvl } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount =
		fromIntegral $ natVal (Proxy :: Proxy c) }

allocate ::
	(WithPoked n, TpLvlLst.Length [Type] vss, HeteroParList.FromList vss) =>
	Device.D sd -> AllocateInfo n scp vss ->
	(forall s . HeteroParList.PL (Binded s) vss -> IO a) -> IO a
allocate (Device.D dvc) ai f = bracket
	(M.allocate dvc $ allocateInfoToMiddle ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPool ai)
	(f . HeteroParList.fromList Binded)

data AllocateInfo n s (vss :: [[Type]]) = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoCommandPool :: CommandPool.C s,
	allocateInfoLevel :: Level }
	deriving Show

allocateInfoToMiddle :: forall n s vss .
	TpLvlLst.Length [Type] vss => AllocateInfo n s vss -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = CommandPool.C cp,
	allocateInfoLevel = lvl } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount =
		fromIntegral (TpLvlLst.length @_ @vss) }

begin :: (WithPoked n, WithPoked n') =>
	Binded s vs -> M.BeginInfo n n' -> IO a -> IO a
begin (Binded cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

beginNew :: (WithPoked n, WithPoked n') =>
	C s -> M.BeginInfo n n' -> IO a -> IO a
beginNew (C cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: Binded sc vs -> ResetFlags -> IO ()
reset (Binded cb) rfs = M.reset cb rfs

resetNew :: C sc -> ResetFlags -> IO ()
resetNew (C cb) rfs = M.reset cb rfs

allocateOld :: WithPoked n =>
	Device.D sd -> AllocateInfoOld n sp ->
	(forall s . [Binded s vs] -> IO a) -> IO a
allocateOld (Device.D dvc) (allocateInfoToMiddleOld -> ai) f = bracket
	(M.allocate dvc ai) (M.freeCs dvc (M.allocateInfoCommandPool ai))
	(f . (Binded <$>))

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
