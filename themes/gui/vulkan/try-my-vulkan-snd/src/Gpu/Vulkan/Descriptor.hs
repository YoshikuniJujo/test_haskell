{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor where

import Data.Kind.Object
import Data.HeteroList

import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Descriptor.Middle as M
import qualified Gpu.Vulkan.Descriptor.Core as C

data BufferInfo objs obj where
	BufferInfoAtom ::
		{ bufferInfoAtomBuffer :: Buffer.B objs } ->
		BufferInfo objs ('Atom v)
	BufferInfoList ::
		{ bufferInfoListBuffer :: Buffer.B objs } ->
		BufferInfo objs ('List v)

deriving instance Show (HeteroVarList ObjectLength objs) =>
	Show (BufferInfo objs obj)

bufferInfoToCore :: Offset obj objs =>
	BufferInfo objs obj -> C.BufferInfo
bufferInfoToCore = M.bufferInfoToCore . bufferInfoToMiddle

bufferInfoToMiddle :: forall objs obj . Offset obj objs =>
	BufferInfo objs obj -> M.BufferInfo
bufferInfoToMiddle BufferInfoAtom {
	bufferInfoAtomBuffer = Buffer.B lns b } = M.BufferInfo {
	M.bufferInfoBuffer = Buffer.M.B b,
	M.bufferInfoOffset = fromIntegral $ offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ range @obj lns }
bufferInfoToMiddle BufferInfoList {
	bufferInfoListBuffer = Buffer.B lns b } = M.BufferInfo {
	M.bufferInfoBuffer = Buffer.M.B b,
	M.bufferInfoOffset = fromIntegral $ offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ range @obj lns }
