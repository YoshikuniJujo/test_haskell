{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer (
	C, allocate, allocateNew, AllocateInfo(..), AllocateInfoNew(..), begin, M.BeginInfo(..), reset ) where

import Foreign.Pointable
import Control.Exception
import Data.Kind
import Data.HeteroList
import Data.Word

import qualified TypeLevel.List as TpLvlLst

import Gpu.Vulkan.CommandBuffer.Type
import Gpu.Vulkan.CommandBuffer.Enum

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.CommandPool.Type as CommandPool
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

allocateInfoToMiddleNew :: AllocateInfoNew n s vss -> M.AllocateInfoNew n vss
allocateInfoToMiddleNew AllocateInfoNew {
	allocateInfoNextNew = nxt,
	allocateInfoCommandPoolNew = CommandPool.C cp,
	allocateInfoLevelNew = lvl } = M.AllocateInfoNew {
	M.allocateInfoNextNew = nxt,
	M.allocateInfoCommandPoolNew = cp,
	M.allocateInfoLevelNew = lvl }

cListFromMiddle :: HeteroVarList M.C vss -> HeteroVarList (C s) vss
cListFromMiddle HVNil = HVNil
cListFromMiddle (cb :...: cbs) = C cb :...: cListFromMiddle cbs

allocateNew ::
	(Pointable n, TpLvlLst.Length [Type] vss, ListToHeteroVarList vss) =>
	Device.D sd -> AllocateInfoNew n scp vss ->
	(forall s . HeteroVarList (C s) vss -> IO a) -> IO a
allocateNew (Device.D dvc) (allocateInfoToMiddleNew -> ai) f = bracket
	(M.allocateNew dvc ai) (M.freeCsNew dvc $ M.allocateInfoCommandPoolNew ai)
	(f . heteroVarListMap C)

{-
allocateNew :: Pointable n =>
	Device.D sd -> AllocateInfo n sp ->
	(forall s vss . HeteroVarList (C s) vss -> IO a) -> IO a
allocateNew (Device.D dvc) (allocateInfoToMiddle -> ai) f = M.allocateNew dvc ai \mcbs ->
	f (cListFromMiddle mcbs) `finally` M.freeCsNew dvc (M.allocateInfoCommandPool ai) mcbs
	-}

allocate :: Pointable n =>
	Device.D sd -> AllocateInfo n sp ->
	(forall s . [C s vs] -> IO a) -> IO a
allocate (Device.D dvc) (allocateInfoToMiddle -> ai) f = bracket
	(M.allocate dvc ai) (M.freeCs dvc (M.allocateInfoCommandPool ai))
	(f . (C . M.C <$>))

begin :: (Pointable n, Pointable n') =>
	C s vs -> M.BeginInfo n n' -> IO a -> IO a
begin (C (M.C cb)) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: C sc vs -> ResetFlags -> IO ()
reset (C (M.C cb)) rfs = M.reset cb rfs
