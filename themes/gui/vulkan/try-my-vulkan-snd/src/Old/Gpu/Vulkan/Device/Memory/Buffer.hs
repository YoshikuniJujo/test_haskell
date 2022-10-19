{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Gpu.Vulkan.Device.Memory.Buffer where

import Prelude hiding (map)

import Foreign.Ptr
import Control.Exception
import Data.Kind.Object
import Data.HeteroList
import Data.IORef

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle.Internal as Device.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Memory.Core as Memory.C

import Old.Gpu.Vulkan.Device.Memory.Buffer.Types
import Old.Gpu.Vulkan.Device.Memory.Buffer.TypeLevel

data M s (objss :: [[Object]]) = M (HeteroVarList Form objss) Memory.C.M

deriving instance Show (HeteroVarList Form objss) => Show (M s objss)

deriving instance Show (HeteroVarList ObjectLength objs) => Show (Form objs)

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
map (Device.D dvc) (M fs m) flgs = do
	let	(ost, sz) = offsetSize @obj 0 fs
	mem <- newIORef m
	Memory.M.map dvc (Device.M.Memory mem) ost sz flgs

unmap :: Device.D sd -> M sm objss -> IO ()
unmap (Device.D dvc) (M _ m) = do
	mem <- newIORef m
	Memory.M.unmap dvc (Device.M.Memory mem)
