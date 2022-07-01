{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Memory.Buffer where

import Prelude hiding (map)

import Foreign.Ptr
import Control.Exception
import Data.Kind.Object
import Data.HeteroList

import Gpu.Vulkan.Device.Memory.Buffer.Types
import Gpu.Vulkan.Device.Memory.Buffer.TypeLevel
import Gpu.Vulkan.Memory

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Device.Core as Device.C
import qualified Gpu.Vulkan.Memory.Middle as Memory.M

data M s (objss :: [[Object]]) =
	M (HeteroVarList Form objss) Device.C.Memory

deriving instance Show (HeteroVarList Form objss) => Show (M s objss)

deriving instance Show (HeteroVarList ObjectLength objs) => Show (Form objs)

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoMemoryTypeIndex :: TypeIndex }
	deriving (Show, Eq)

write :: forall obj v objss sd sm .
	(StoreObject v obj, OffsetSize obj objss) =>
	Device.D sd -> M sm objss -> Memory.M.MapFlags -> v -> IO ()
write dvc mem@(M fs _) flgs v = bracket
	(map @obj dvc mem flgs) (const $ unmap dvc mem)
	(\(ptr :: Ptr (ObjectType obj)) ->
		storeObject @_ @obj ptr (offsetSizeLength fs) v)

read :: forall v obj objss sd sm .
	(StoreObject v obj, OffsetSize obj objss) =>
	Device.D sd -> M sm objss -> Memory.M.MapFlags -> IO v
read dvc mem@(M fs _) flgs = bracket
	(map @obj dvc mem flgs) (const $ unmap dvc mem)
	(\ptr -> loadObject @_ @obj ptr (offsetSizeLength fs))

map :: forall obj objss sd sm . OffsetSize obj objss =>
	Device.D sd -> M sm objss -> Memory.M.MapFlags ->
	IO (Ptr (ObjectType obj))
map (Device.D dvc) (M fs mem) flgs = do
	let	(ost, sz) = offsetSize @obj 0 fs
	Memory.M.map dvc (Device.M.Memory mem) ost sz flgs

unmap :: Device.D sd -> M sm objss -> IO ()
unmap (Device.D dvc) (M _ mem) = Memory.M.unmap dvc (Device.M.Memory mem)
