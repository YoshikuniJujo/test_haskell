{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer (

	-- * ALLOCATE

	allocate, C, GBinded, CBinded, AllocateInfo(..),
	allocateNew, AllocateInfoNew(..),

	-- * BEGIN AND RESET

	begin, reset, M.BeginInfo(..), M.InheritanceInfo(..)

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TLength
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.CommandBuffer.Type
import Gpu.Vulkan.CommandBuffer.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.CommandPool.Type as CommandPool
import qualified Gpu.Vulkan.CommandBuffer.Middle as M

allocate :: (
	WithPoked (TMaybe.M mn), TLength.Length c, HeteroParList.FromList c ) =>
	Device.D sd -> AllocateInfo mn scp c ->
	(forall scb . HeteroParList.LL (C scb) c -> IO a) -> IO a
allocate (Device.D dvc) ai f = bracket
	(M.allocateCs dvc $ allocateInfoToMiddle ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPool ai)
	(f . HeteroParList.fromList (HeteroParList.Dummy . C))

allocateNew :: WithPoked (TMaybe.M mn) =>
	Device.D sd -> AllocateInfoNew mn scp ->
	(forall scb . [C scb] -> IO a) -> IO a
allocateNew (Device.D dvc) ai f = bracket
	(M.allocateCs dvc $ allocateInfoToMiddleNew ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPoolNew ai)
	(f . (C <$>))

data AllocateInfo mn scp (c :: [()]) = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoCommandPool :: CommandPool.C scp,
	allocateInfoLevel :: Level }

data AllocateInfoNew mn scp = AllocateInfoNew {
	allocateInfoNextNew :: TMaybe.M mn,
	allocateInfoCommandPoolNew :: CommandPool.C scp,
	allocateInfoLevelNew :: Level,
	allocateInfoCommandBufferCountNew :: Word32 }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn s c)

allocateInfoToMiddle :: forall n s c . TLength.Length c =>
	AllocateInfo n s c -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoCommandPool = CommandPool.C cp,
	allocateInfoLevel = lvl } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount = TLength.length @_ @c }

allocateInfoToMiddleNew :: AllocateInfoNew n s -> M.AllocateInfo n
allocateInfoToMiddleNew AllocateInfoNew {
	allocateInfoNextNew = mnxt,
	allocateInfoCommandPoolNew = CommandPool.C cp,
	allocateInfoLevelNew = lvl,
	allocateInfoCommandBufferCountNew = c } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount = c }

begin :: (WithPoked (TMaybe.M mn), WithPoked (TMaybe.M ii)) =>
	C s -> M.BeginInfo mn ii -> IO a -> IO a
begin (C cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: C sc -> ResetFlags -> IO ()
reset (C cb) rfs = M.reset cb rfs
