{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer (
	C, allocate, AllocateInfo(..), begin, M.BeginInfo(..), M.beginInfoNil, reset ) where

import Foreign.Pointable
import Control.Exception
import Data.Word

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

allocate :: Pointable n =>
	Device.D sd -> AllocateInfo n sp ->
	(forall s . [C s vs] -> IO a) -> IO a
allocate (Device.D dvc) (allocateInfoToMiddle -> ai) f = bracket
	(M.allocate dvc ai) (M.freeCs dvc (M.allocateInfoCommandPool ai))
	(f . (C <$>))

begin :: (Pointable n, Pointable n') =>
	C s vs -> M.BeginInfo n n' -> IO a -> IO a
begin (C cb) bi act = bracket_ (M.begin cb bi) (M.end cb) act

reset :: C sc vs -> ResetFlags -> IO ()
reset (C cb) rfs = M.reset cb rfs
