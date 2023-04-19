{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Proxy
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import qualified TypeLevel.List as TpLvlLst

import Gpu.Vulkan.CommandBuffer.Type
import Gpu.Vulkan.CommandBuffer.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.CommandPool.Type as CommandPool
import qualified Gpu.Vulkan.CommandBuffer.Middle as M

import qualified Gpu.Vulkan.VertexInput as VertexInput

allocateNew :: (
	WithPoked (TMaybe.M mn), KnownNat c,
	HeteroParList.FromList (HeteroParList.Dummies c) ) =>
	Device.D sd -> AllocateInfoNew mn scp c ->
	(forall s . HeteroParList.LL' (C s) c -> IO a) -> IO a
allocateNew (Device.D dvc) ai f = bracket
	(M.allocate dvc $ allocateInfoToMiddleNew ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPoolNew ai)
	(f . HeteroParList.fromList (HeteroParList.Dummy . C))

data AllocateInfoNew mn s (c :: Nat) = AllocateInfoNew {
	allocateInfoNextNew :: TMaybe.M mn,
	allocateInfoCommandPoolNew :: CommandPool.C s,
	allocateInfoLevelNew :: Level }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfoNew mn s c)

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
	(WithPoked (TMaybe.M mn), TpLvlLst.Length [(Type, VertexInput.Rate)] vss, HeteroParList.FromList vss) =>
	Device.D sd -> AllocateInfo mn scp vss ->
	(forall s . HeteroParList.PL (Binded s) vss -> IO a) -> IO a
allocate (Device.D dvc) ai f = bracket
	(M.allocate dvc $ allocateInfoToMiddle ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPool ai)
	(f . HeteroParList.fromList Binded)

data AllocateInfo mn s (vss :: [[(Type, VertexInput.Rate)]]) = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoCommandPool :: CommandPool.C s,
	allocateInfoLevel :: Level }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn s vss)

allocateInfoToMiddle :: forall n s vss .
	TpLvlLst.Length [(Type, VertexInput.Rate)] vss => AllocateInfo n s vss -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = CommandPool.C cp,
	allocateInfoLevel = lvl } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount =
		fromIntegral (TpLvlLst.length @_ @vss) }

begin :: (WithPoked (TMaybe.M mn), WithPoked (TMaybe.M mn')) =>
	Binded s vs -> M.BeginInfo mn mn' -> IO a -> IO a
begin (Binded cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

beginNew :: (WithPoked (TMaybe.M mn), WithPoked (TMaybe.M mn')) =>
	C s -> M.BeginInfo mn mn' -> IO a -> IO a
beginNew (C cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: Binded sc vs -> ResetFlags -> IO ()
reset (Binded cb) rfs = M.reset cb rfs

resetNew :: C sc -> ResetFlags -> IO ()
resetNew (C cb) rfs = M.reset cb rfs

allocateOld :: WithPoked (TMaybe.M mn) =>
	Device.D sd -> AllocateInfoOld mn sp ->
	(forall s . [Binded s vs] -> IO a) -> IO a
allocateOld (Device.D dvc) (allocateInfoToMiddleOld -> ai) f = bracket
	(M.allocate dvc ai) (M.freeCs dvc (M.allocateInfoCommandPool ai))
	(f . (Binded <$>))

data AllocateInfoOld mn s = AllocateInfoOld {
	allocateInfoNextOld :: TMaybe.M mn,
	allocateInfoCommandPoolOld :: CommandPool.C s,
	allocateInfoLevelOld :: Level,
	allocateInfoCommandBufferCountOld :: Word32 }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfoOld mn s)

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
