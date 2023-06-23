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

	allocateNew, C, Binded, AllocateInfoNew(..),

	-- * BEGIN AND RESET

	beginNew, resetNew, M.BeginInfo(..)

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.List qualified as TLength
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.CommandBuffer.Type
import Gpu.Vulkan.CommandBuffer.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.CommandPool.Type as CommandPool
import qualified Gpu.Vulkan.CommandBuffer.Middle as M

import qualified Gpu.Vulkan.VertexInput as VertexInput

allocateNew :: (
	WithPoked (TMaybe.M mn), HeteroParList.FromList c, TLength.Length c ) =>
	Device.D sd -> AllocateInfoNew mn scp c ->
	(forall s . HeteroParList.LL (C s) c -> IO a) -> IO a
allocateNew (Device.D dvc) ai f = bracket
	(M.allocateCs dvc $ allocateInfoToMiddleNew ai)
	(M.freeCs dvc
		. (\(CommandPool.C cp) -> cp) $ allocateInfoCommandPoolNew ai)
	(f . HeteroParList.fromList (HeteroParList.Dummy . C))

data AllocateInfoNew mn s (c :: [()]) = AllocateInfoNew {
	allocateInfoNextNew :: TMaybe.M mn,
	allocateInfoCommandPoolNew :: CommandPool.C s,
	allocateInfoLevelNew :: Level }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfoNew mn s c)

allocateInfoToMiddleNew :: forall n s c . TLength.Length c =>
	AllocateInfoNew n s c -> M.AllocateInfo n
allocateInfoToMiddleNew AllocateInfoNew {
	allocateInfoNextNew = mnxt,
	allocateInfoCommandPoolNew = CommandPool.C cp,
	allocateInfoLevelNew = lvl } = M.AllocateInfo {
	M.allocateInfoNext = mnxt,
	M.allocateInfoCommandPool = cp,
	M.allocateInfoLevel = lvl,
	M.allocateInfoCommandBufferCount = TLength.length @_ @c }

data AllocateInfo mn s (vss :: [[(Type, VertexInput.Rate)]]) = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoCommandPool :: CommandPool.C s,
	allocateInfoLevel :: Level }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn s vss)

beginNew :: (WithPoked (TMaybe.M mn), WithPoked (TMaybe.M mn')) =>
	C s -> M.BeginInfo mn mn' -> IO a -> IO a
beginNew (C cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

resetNew :: C sc -> ResetFlags -> IO ()
resetNew (C cb) rfs = M.reset cb rfs
