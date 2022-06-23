{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor where

import Data.Kind
import Data.Kind.Object
import Data.HeteroList

import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Descriptor.Middle as M
import qualified Gpu.Vulkan.Descriptor.Core as C

data BufferInfo (sbsmobjsobj :: BufferInfoArg) where
	BufferInfoAtom ::
		{ bufferInfoAtomBuffer :: Buffer.Binded sb sm objs } ->
		BufferInfo '(sb, sm, objs, 'Atom v)
	BufferInfoList ::
		{ bufferInfoListBuffer :: Buffer.Binded sb sm objs } ->
		BufferInfo '(sb, sm, objs, 'List v)

type BufferInfoArg = (Type, Type, [Object], Object)

deriving instance Show (HeteroVarList ObjectLength objs) =>
	Show (BufferInfo '(sb, sm, objs, obj))

bufferInfoToCore :: Offset obj objs =>
	BufferInfo '(sb, sm, objs, obj) -> C.BufferInfo
bufferInfoToCore = M.bufferInfoToCore . bufferInfoToMiddle

bufferInfoToMiddle :: forall sb sm objs obj . Offset obj objs =>
	BufferInfo '(sb, sm, objs, obj) -> M.BufferInfo
bufferInfoToMiddle BufferInfoAtom {
	bufferInfoAtomBuffer = Buffer.Binded lns b } = M.BufferInfo {
	M.bufferInfoBuffer = Buffer.M.B b,
	M.bufferInfoOffset = fromIntegral $ offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ range @obj lns }
bufferInfoToMiddle BufferInfoList {
	bufferInfoListBuffer = Buffer.Binded lns b } = M.BufferInfo {
	M.bufferInfoBuffer = Buffer.M.B b,
	M.bufferInfoOffset = fromIntegral $ offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ range @obj lns }
