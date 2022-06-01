{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer (C, allocate, AllocateInfo(..)) where

import Foreign.Pointable
import Control.Exception
import Data.Word

import Vulkan.CommandBuffer.Enum

import qualified Vulkan.Device.Type as Device
import qualified Vulkan.CommandPool.Type as CommandPool
import qualified Vulkan.CommandBuffer.Middle as M

newtype C s vs = C (M.C vs) deriving Show

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

allocate :: Pointable n =>
	Device.D sd -> AllocateInfo n sp ->
	(forall s . [C s vs] -> IO a) -> IO a
allocate (Device.D dvc) (allocateInfoToMiddle -> ai) f = bracket
	(M.allocate dvc ai) (M.freeCs dvc (M.allocateInfoCommandPool ai))
	(f . (C <$>))
